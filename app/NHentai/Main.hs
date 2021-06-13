{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Error
import Control.Lens
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Trans.Control
import Data.Aeson hiding (json)
import Data.Char
import Data.NHentai.API.Gallery
import Data.NHentai.Scraper.HomePage
import Data.NHentai.Scraper.Types
import Data.NHentai.Types
import Data.Time
import Data.Time.Format.ISO8601
import Data.Version (showVersion)
import NHentai.Options
import NHentai.Utils
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status
import Options.Applicative
import Paths_nhentai (version)
import Refined
import Streaming (Stream, Of)
import System.Directory
import System.FilePath
import System.IO
import Text.HTML.Scalpel.Core
import Text.URI
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Streaming.Concurrent as S
import qualified Streaming.Prelude as S

class HasManager a where
  manager :: Lens' a Manager

instance HasManager Manager where
  manager = id

requestFromModernURI :: (MonadLoggerIO m, MonadCatch m, MonadIO m, HasManager cfg) => cfg -> URI -> m BL.ByteString
requestFromModernURI cfg uri = do
  req <- parseRequest (renderStr uri)
  let
    req' = req
      { responseTimeout = responseTimeoutNone
      }

  rep <- fix $ \loop -> do
    rep <- (liftIO $ httpLbs req' (cfg ^. manager)) `catch` \(e :: SomeException) -> do
      $logError $ "redownload " <> render uri <> " due to error occurred when doing http request: " <> T.pack (show e)
      loop

    if responseStatus rep == serviceUnavailable503 then do
      $logError $ "redownload " <> render uri <> " due to abnormal status code 503 (service unavailable)"
      loop
    else do
      pure rep

  pure $ responseBody rep

getLatestGalleryId :: (MonadLoggerIO m, MonadCatch m, MonadIO m) => Manager -> m GalleryId
getLatestGalleryId manager = do
  uri <- mkHomePageUri $$(refineTH 1)
  body <- requestFromModernURI manager uri
  case scrapeStringLike body homePageScraper of
    Nothing -> throwM $ ScalpelException body
    Just home_page -> pure $ home_page ^. recentGalleries . head1 . scraperGalleryId

data Config
  = Config
    { _cfgManager :: Manager
    , _cfgDownloadWarningOptions :: DownloadWarningOptions
    , _cfgOutputConfig :: OutputConfig
    , _cfgDownloadOptions :: DownloadOptions
    }

makeLenses ''Config

instance HasDownloadWarningOptions Config where
  downloadWarningOptions = cfgDownloadWarningOptions

instance HasOutputConfig Config where
  outputConfig = cfgOutputConfig

instance HasManager Config where
  manager = cfgManager

instance HasDownloadOptions Config where
  downloadOptions = cfgDownloadOptions

data Download = Download URI FilePath
  deriving (Show, Eq)

runDownload :: (MonadCatch m, MonadLoggerIO m, HasManager cfg, HasDownloadWarningOptions cfg) => cfg -> Download -> m ()
runDownload cfg (Download uri dest_path) = do
  file_exist <- liftIO $ doesFileExist dest_path
  if file_exist then do
    $logInfo $ "skipped downloading " <> download_info <> ": file exists"
  else do
    $logDebug $ "downloading " <> download_info

    (duration, body) <- withTimer $ requestFromModernURI (cfg ^. manager) uri

    let body_length = BL.length body
    $logInfo $ "downloaded " <> download_info <> ": took " <> T.pack (show duration) <> ", size: " <> byteSizeToText body_length

    if_just (cfg ^. downloadWarnMinSize) $ \least_size -> do
      when (body_length <= unrefine least_size) $ do
        $logWarn $ "downloaded " <> download_info <> ", but: downloaded content's size (" <> byteSizeToText body_length <> ") is too small (<= " <> byteSizeToText (unrefine least_size) <> "), the content may be invalid"

    if_just (cfg ^. downloadWarnMinDuration) $ \min_duration -> do
      when (duration > min_duration) $ do
        $logWarn $ "downloaded " <> download_info <> ", but: download time (" <> T.pack (show duration) <> ") is too long (>= " <> T.pack (show min_duration) <> ")"

    liftIO $ do
      createDirectoryIfMissing True (takeDirectory dest_path)
      BL.writeFile dest_path body
  where
  if_just (Just v) m = m v
  if_just Nothing _ = pure ()

  download_info = render uri <> " -> " <> T.pack dest_path

fetchGallery
  ::
  ( MonadCatch m
  , MonadLoggerIO m
  , HasOutputConfig cfg
  , HasManager cfg
  , HasDownloadWarningOptions cfg
  , HasDownloadOptions cfg
  )
  => cfg
  -> GalleryId
  -> Stream (Of Download) m ()
fetchGallery cfg gid = do
  gallery_api_uri <- mkGalleryApiUri gid

  runDownload cfg (Download gallery_api_uri gallery_json_path)
  body <- liftIO $ BL.readFile gallery_json_path

  case eitherDecode @APIGalleryResult body of
    Left err -> do
      $logError $ "redownload gallery " <> T.pack (show $ unrefine gid) <> "unable to decode JSON from gallery " <> T.pack (show $ unrefine gid) <> ", body: " <> T.pack (show body) <> ": " <> T.pack err
      liftIO $ removeFile gallery_json_path
      fetchGallery cfg gid

    Right (APIGalleryResultError _) -> do
      $logWarn $ "skip gallery " <> T.pack (show $ unrefine gid) <> ": gallery is dead"
      pure ()

    Right (APIGalleryResultSuccess g) -> extractDownloads g
  where
  gallery_json_path = (cfg ^. jsonPathMaker) gid
  extractDownloads g = S.for (enumerate $ S.each $ (g ^. pages)) phi
    where
    phi (unref_pageidx :: Int, page) = lift (refineThrow unref_pageidx) >>= \pageidx -> case page ^. eitherImageType of
      Left string -> do
        lift $ $logWarn $ "Image type is "
          <> T.pack (show string)
          <> ": Gallery: "
          <> T.pack (show $ unrefine $ g ^. galleryId)
          <> ", page: "
          <> T.pack (show $ unrefine pageidx)
          <> ", the image is probably invalid, skipping"
        pure ()
      Right imgtype -> do
        let
          f flag_lens mk_uri path_maker_lens = when (cfg ^. flag_lens) $ do
            uri <- lift $ mk_uri (g ^. mediaId) pageidx imgtype
            S.yield $ Download uri ((cfg ^. path_maker_lens) (g ^. galleryId) (g ^. mediaId) pageidx imgtype)

        f downloadPageThumbnailFlag mkPageThumbnailUri pageThumbPathMaker
        f downloadPageImageFlag mkPageImageUri pageImagePathMaker

runMainOptions :: (MonadMask m, MonadBaseControl IO m, MonadLoggerIO m) => MainOptions -> m ()
runMainOptions (MainOptionsDownload {..}) = do
  mgr <- newTlsManager
  let
    cfg = Config
      { _cfgManager = mgr
      , _cfgOutputConfig = outputConfig'MainOptionsDownload
      , _cfgDownloadOptions = downloadOptions'MainOptionsDownload
      , _cfgDownloadWarningOptions = downloadWarningOptions'MainOptionsDownload
      }
  duration <- withTimer_ $ run cfg gidInputOption'MainOptionsDownload
  $logInfo $ "done downloading all galleries, time taken: " <> T.pack (show duration)
  where
  run cfg (GidInputOptionSingle gid) = download_gids cfg (S.yield gid)
  run cfg (GidInputOptionListFile file_path) = do
    bracket (liftIO $ openFile file_path ReadMode) (liftIO . hClose) $ \h -> do
      let gid_stream = S.mapMaybeM parse $ enumerate $ S.filter (not . null) $ S.fromHandle $ h
      download_gids cfg gid_stream
    where
    parse (line_at :: Integer, string) = case readMay string of
      Nothing -> do
        $logWarn $ prefix <> " unable to parse " <> T.pack (show string) <> " as a gallery id, skipping"
        pure Nothing
      Just unref_gid -> case refineThrow unref_gid of
        Left err -> do
          $logError $ prefix <> " unable to refine " <> T.pack (show unref_gid) <> " into a gallery id: " <> T.pack (show err)
          pure Nothing
        Right gid -> pure $ Just (gid :: GalleryId)
      where
      prefix = "at " <> T.pack file_path <> ":" <> T.pack (show line_at) <> ":"

  download_gids cfg gid_stream = do
    S.withStreamMapM
      (unrefine numThreads'MainOptionsDownload)
      (runDownload cfg)
      (S.for gid_stream (fetchGallery cfg))
      S.effects

runMainOptions MainOptionsVersion = do
  liftIO $ putStrLn (showVersion version)
runMainOptions MainOptionsLatestGid = do
  manager <- newTlsManager
  latest_gid <- getLatestGalleryId manager
  liftIO $ putStrLn $ show (unrefine latest_gid)

main :: IO ()
main = do
  options <- execParser $ info (programOptionsParser <**> helper)
    ( fullDesc
    <> progDesc "A scraper/downloader for nhentai.net"
    )
  let
    filtered = filterLogger
      (\_ level -> case maybeLogLevel'ProgramOptions options of
        Nothing -> False
        Just level' -> level' <= level
      )
      (runMainOptions (mainOptions'ProgramOptions options))
  runLoggingT filtered $ \loc source level logstr -> do
    time <- getCurrentTime
    let line = toLogStr (iso8601Show time) <> ": [" <> logLevelToLogStr level <> "] " <> toLogStr logstr <> "\n"
    BSC.hPutStr stderr $ fromLogStr line
  where
  logLevelToLogStr level = toLogStr $ toLower <$> drop 5 (show level)
