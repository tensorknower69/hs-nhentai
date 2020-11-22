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
import Data.NHentai.API.Gallery
import Data.NHentai.Scraper.HomePage
import Data.NHentai.Scraper.Types
import Data.NHentai.Types
import Data.Time
import Data.Time.Format.ISO8601
import NHentai.Options
import NHentai.Utils
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status
import Options.Applicative
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
	let req' = req
		{ responseTimeout = responseTimeoutNone
		}

	rep <- fix $ \loop -> do
		rep <- (liftIO $ httpLbs req' (cfg ^. manager)) `catch` \(e :: SomeException) -> do
			$logError $ "Redownloading, error when doing request: " <> render uri <> "\n- Error: " <> T.pack (show e)
			loop

		if responseStatus rep == serviceUnavailable503 then do
			$logError $ "Redownloading, abnormal status code: 503, URI: " <> render uri
			loop
		else do
			pure rep

	pure $ responseBody rep

getLatestGalleryId :: (MonadLoggerIO m, MonadCatch m, MonadIO m, HasManager cfg) => cfg -> m GalleryId
getLatestGalleryId cfg = do
	uri <- mkHomePageUri $$(refineTH 1)
	body <- requestFromModernURI cfg uri
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
		$logDebug $ "Skipped downloading: " <> arrow_text <> ", file exists"
	else do
		$logDebug $ "Downloading: " <> arrow_text <> "..."

		(dt, body) <- withTimer $ requestFromModernURI (cfg ^. manager) uri

		let body_length = BL.length body
		$logDebug $ "Downloaded: " <> arrow_text <> ", took " <> T.pack (show dt) <> ", byte length: " <> T.pack (show body_length)

		if_just (cfg ^. downloadWarnLeastSize) $ \least_size -> do
			when (body_length <= unrefine least_size) $ do
				$logWarn $ "Downloaded content's size is too small: "
					<> T.pack (show body_length)
					<> " (<= "
					<> T.pack (show $ unrefine least_size)
					<> " bytes): "
					<> arrow_text
					<> ", the content may be invalid or not"

		if_just (cfg ^. downloadWarnMostDuration) $ \most_dt -> do
			when (dt > most_dt) $ do
				$logWarn $ "Downloading time took too long: "
					<> T.pack (show dt)
					<> " (>= "
					<> T.pack (show most_dt)
					<> "): "
					<> arrow_text

		liftIO $ do
			createDirectoryIfMissing True (takeDirectory dest_path)
			BL.writeFile dest_path body
	where
	if_just (Just v) m = m v
	if_just Nothing _ = pure ()
	arrow_text = render uri <> " -> " <> T.pack dest_path

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
			$logError $ "Unable to decode JSON from gallery: " <> T.pack (show $ unrefine gid) <> "\n- Error: " <> T.pack err <> "\n- Body: " <> T.pack (show body)
			$logInfo $ "Redownloading " <> T.pack (show $ unrefine gid) <> "..."
			liftIO $ removeFile gallery_json_path
			fetchGallery cfg gid

		Right (APIGalleryResultError _) -> do
			$logWarn $ "Gallery is dead: " <> T.pack (show $ unrefine gid) <> ", skipping"
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
				let f flag_lens mk_uri path_maker_lens = when (cfg ^. flag_lens) $ do
					uri <- lift $ mk_uri (g ^. mediaId) pageidx imgtype
					S.yield $ Download uri ((cfg ^. path_maker_lens) (g ^. galleryId) (g ^. mediaId) pageidx imgtype)

				f downloadPageThumbnailFlag mkPageThumbnailUri pageThumbPathMaker
				f downloadPageImageFlag mkPageImageUri pageImagePathMaker

runMainOptions :: (MonadMask m, MonadBaseControl IO m, MonadLoggerIO m) => MainOptions -> m ()
runMainOptions (MainOptionsDownload {..}) = do
	mgr <- newTlsManager
	let cfg = Config
		{ _cfgManager = mgr
		, _cfgOutputConfig = outputConfig'MainOptionsDownload
		, _cfgDownloadOptions = downloadOptions'MainOptionsDownload
		, _cfgDownloadWarningOptions = downloadWarningOptions'MainOptionsDownload
		}
	dt <- withTimer_ $ run cfg gidInputOption'MainOptionsDownload
	$logInfo $ "Done downloading all galleries! Time taken: " <> T.pack (show dt)
	where
	run cfg (GidInputOptionSingle gid) = download_gids cfg (S.yield gid)
	run cfg (GidInputOptionListFile file_path) = do
		bracket (liftIO $ openFile file_path ReadMode) (liftIO . hClose) $ \h -> do
			let gid_stream = S.mapMaybeM parse $ enumerate $ S.filter (not . null) $ S.fromHandle $ h
			download_gids cfg gid_stream
		where
		parse (line_at :: Integer, string) = case readMay string of
			Nothing -> do
				$logWarn $ prefix <> "Unable to parse " <> T.pack (show string) <> " as a gallery id, skipping"
				pure Nothing
			Just unref_gid -> case refineThrow unref_gid of
				Left err -> do
					$logError $ prefix <> "Unable to refine " <> T.pack (show unref_gid) <> " to a gallery id, skipping. Error: " <> T.pack (show err)
					pure Nothing
				Right gid -> pure $ Just (gid :: GalleryId)
			where
			prefix = "In " <> T.pack file_path <> ":" <> T.pack (show line_at) <> ": "
	download_gids cfg gid_stream = do
		S.withStreamMapM
			(unrefine numThreads'MainOptionsDownload)
			(runDownload cfg)
			(S.for gid_stream (fetchGallery cfg))
			S.effects

runMainOptions MainOptionsVersion = liftIO $ putStrLn "0.1.3.0"
runMainOptions MainOptionsLatestGid = do
	mgr <- newTlsManager
	latest_gid <- getLatestGalleryId mgr
	liftIO $ putStrLn $ show (unrefine latest_gid)

main :: IO ()
main = do
	options <- execParser $ info (programOptionsParser <**> helper)
		( fullDesc
		<> progDesc "A scraper/downloader for nhentai.net"
		)
	let filtered = filterLogger
		(\_ level -> case maybeLogLevel'ProgramOptions options of
			Nothing -> False
			Just level' -> level' <= level
		)
		$ runMainOptions (mainOptions'ProgramOptions options)
	runLoggingT filtered $ \loc source level logstr -> do
		let lvl_name = toLogStr $ drop 5 (show level)
		t <- getCurrentTime
		let line = toLogStr (iso8601Show t) <> ": " <> lvl_name <> ": " <> toLogStr logstr <> "\n"
		BSC.hPutStr stderr $ fromLogStr line
