{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Error
import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Aeson hiding (json)
import Data.List.Split
import Data.NHentai.API.Gallery
import Data.NHentai.Scraper.HomePage
import Data.NHentai.Scraper.Types
import Data.NHentai.Types
import Data.Time.Clock.POSIX
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Options.Applicative
import Options.Applicative.Types
import Refined
import Streaming as S
import System.Directory
import System.FilePath
import Text.HTML.Scalpel hiding (Config)
import Text.URI
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Streaming.Concurrent as S
import qualified Streaming.Prelude as S

-- i honestly think that this is not a good idea
instance (Functor f, MonadThrow m) => MonadThrow (Stream f m)
instance (Functor f, MonadLogger m) => MonadLogger (Stream f m)
instance (Functor f, MonadLoggerIO m) => MonadLoggerIO (Stream f m)

getRecentGalleryId :: IO GalleryID
getRecentGalleryId = do
	url <- mkHomePageUrl $$(refineTH 1)
	scrapeURL @String (renderStr url) homePageScraper >>= \case
		Nothing -> fail $ "unable to scrap " <> show url
		Just may_page -> case may_page of
			Nothing -> fail "invalid page"
			Just page -> case recentGalleries'HomePage page of
				[] -> fail "no recent galleries found"
				(x:_) -> pure $ galleryId'ScraperGallery x

data Config
	= Config
		{ getGalleryJsonPath'Config :: GalleryID -> FilePath
		, getGalleryPageThumbPath'Config :: GalleryID -> MediaID -> PageIndex -> ImageType -> FilePath
		}

initConfig :: Config
initConfig = Config
	{ getGalleryJsonPath'Config = \gid -> "galleries" </> show (unrefine gid) </> "api.json"
	, getGalleryPageThumbPath'Config = \gid _ pid img_type -> "galleries" </> show (unrefine gid) </> (show (unrefine pid) <> "." <> imageTypeExtension img_type)
	}

data AesonParseException = AesonParseException String
	deriving (Show, Eq)
instance Exception AesonParseException

data RunOptions
	= RunDownload
		{ galleryIdList'RunDownload :: [GalleryID]
		, numThreads'RunDownload :: Refined Positive Int
		}
	deriving (Show, Eq)

refineReadM :: (Read x, Predicate p x) => ReadM (Refined p x)
refineReadM = eitherReader $ \string -> do
	case readMay string of
		Nothing -> Left $ "unable to parse string: " <> string
		Just x -> refine x & _Left %~ show

listReadM :: ReadM x -> ReadM [x]
listReadM (ReadM reader') = ReadM . ReaderT $ traverse (runReaderT reader') . splitOn ","

runOptionsParser :: Parser RunOptions
runOptionsParser = RunDownload
	<$> option (listReadM refineReadM) (short 'g' <> long "gallery-ids" <> metavar "GALLERY_IDS")
	<*> option refineReadM (short 't' <> long "num-threads" <> metavar "NUM_THREADS" <> value $$(refineTH @Positive @Int 1))

data ReadException = ReadException { input'ReadException :: String, part'ReadException :: String }
	deriving (Show, Eq)
instance Exception ReadException

programOptionsParser :: ParserInfo RunOptions
programOptionsParser = info (runOptionsParser <**> helper) $
	fullDesc
	<> header "nhentai-cli - a command line interface for nhentai.net written in Haskell"

download :: (MonadThrow m, MonadLoggerIO m) => Manager -> URI -> FilePath -> m ()
download mgr uri dest_path = do
	req <- parseRequest (renderStr uri)
	$logInfo $ "Downloading: " <> arrow_str <> "..."
	t <- liftIO getPOSIXTime
	body <- responseBody <$> liftIO (httpLbs req mgr)
	t' <- liftIO getPOSIXTime
	let dt = t' - t
	$logInfo $ "Downloaded: " <> arrow_str <> ", took " <> T.pack (show dt) <> ", byte length: " <> T.pack (show $ BL.length body)
	liftIO $ createDirectoryIfMissing True (takeDirectory dest_path)
	liftIO $ BL.writeFile dest_path body
	where
	arrow_str = render uri <> " -> " <> T.pack dest_path

fetch :: (MonadThrow m, MonadLoggerIO m) => Manager -> URI -> FilePath -> m BL.ByteString
fetch mgr uri dest_path = do
	exist <- liftIO $ doesFileExist dest_path
	if exist then do
		$logInfo $ "Skipped downloading: " <> arrow_str <> ", file exists"
	else do
		download mgr uri dest_path
	liftIO $ BL.readFile dest_path
	where
	arrow_str = render uri <> " -> " <> T.pack dest_path

data Download = Download URI FilePath
	deriving (Show, Eq)

runDownload :: (MonadThrow m, MonadLoggerIO m) => Manager -> Download -> m ()
runDownload mgr (Download url dest_path) = download mgr url dest_path

fetchGallery :: (MonadThrow m, MonadLoggerIO m) => Config -> Manager -> GalleryID -> Stream (Of Download) m ()
fetchGallery conf mgr gid = do
	gallery_api_url <- mkGalleryApiUrl gid
	(eitherDecode @APIGallery <$> fetch mgr gallery_api_url gallery_json_path) >>= \case
		Left error_string -> throwM (AesonParseException error_string)
		Right json -> do
			downloads <- extractDownloads json
			S.each downloads
	where
	gallery_json_path = getGalleryJsonPath'Config conf gid
	extractDownloads json = catMaybes <$> maybe_list
		where
		maybe_list = forM (zip [1..] (pages'APIGallery json)) $ \(pid', page_thumb) -> do
			pid <- refineThrow pid'
			let image_type = type'ImageSpec page_thumb
			let page_thumb_path = getGalleryPageThumbPath'Config conf gid mid pid image_type
			page_thumb_exist <- liftIO $ doesFileExist page_thumb_path
			if page_thumb_exist then do
				pure $ Nothing
			else do
				page_thumb_url <- mkPageThumbUrl mid pid image_type
				pure $ Just $ Download page_thumb_url page_thumb_path
		mid = mediaId'APIGallery json

mainRun :: (MonadMask m, MonadBaseControl IO m, MonadLoggerIO m) => RunOptions -> m ()
mainRun (RunDownload {..}) = do
	mgr <- liftIO $ newManager tlsManagerSettings
	S.withStreamMapM
		(unrefine numThreads'RunDownload)
		(runDownload mgr)
		(S.for (S.each galleryIdList'RunDownload) (fetchGallery conf mgr))
		S.effects
	where
	conf = initConfig

main :: IO ()
main = do
	run_options <- execParser programOptionsParser
	runStdoutLoggingT $ mainRun run_options
