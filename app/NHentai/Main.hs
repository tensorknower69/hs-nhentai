{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Trans.Control
import Data.Aeson hiding (json)
import Data.NHentai.API.Gallery
import Data.NHentai.Scraper.HomePage
import Data.NHentai.Scraper.Types
import Data.NHentai.Types
import Data.Time.Clock.POSIX
import NHentai.Utils
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Options.Applicative
import Refined
import Streaming as S
import System.Directory
import System.FilePath
import Text.HTML.Scalpel hiding (Config)
import Text.URI
import qualified Data.ByteString.Lazy as BL
import qualified Data.List.NonEmpty as L
import qualified Data.Text as T
import qualified Streaming.Concurrent as S
import qualified Streaming.Prelude as S

getLatestGalleryId :: (MonadThrow m, MonadIO m) => m GalleryID
getLatestGalleryId = do
	url <- mkHomePageUrl $$(refineTH 1)
	home_page' <- liftIO $ scrapeURL @String (renderStr url) homePageScraper
	case home_page' of
		Nothing -> throwM ScalpelException
		Just home_page -> pure . galleryId'ScraperGallery . L.head $ recentGalleries'HomePage home_page

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

data GalleryIDListing
	= GIDListingList
		{ galleryIdList'GIDListingList :: [GalleryID]
		}
	| GIDListingAll
	deriving (Show, Eq)

gidListingOption :: Parser GalleryIDListing
gidListingOption = list_option <|> all_option
	where
	list_option = GIDListingList
		<$> option (listReadM refineReadM)
			( short 'g'
			<> long "gallery-ids"
			<> metavar "GALLERY_IDS"
			)
	all_option = flag' GIDListingAll
		( short 'a'
		<> long "all"
		<> help "download galleries from the latest one to the beginning"
		)

data MainOption
	= MainOptionDownload
		{ galleryIdListing'MainOptionDownload :: GalleryIDListing
		, numThreads'MainOptionDownload :: Refined Positive Int
		}
	deriving (Show, Eq)

mainOptionParser :: Parser MainOption
mainOptionParser = MainOptionDownload
	<$> gidListingOption
	<*> num_threads_option
	where
	num_threads_option = option refineReadM
		( short 't'
		<> long "num-threads"
		<> metavar "NUM_THREADS"
		<> value $$(refineTH @Positive @Int 1)
		)

programOptionsParser :: ParserInfo MainOption
programOptionsParser = info (mainOptionParser <**> helper) $
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
	extractDownloads (APIGallery {..}) = my_list 1 pages'APIGallery
		where
		my_list page_index' (page_image_spec : rest) = do
			page_index <- refineThrow page_index'
			let page_thumb_path = getGalleryPageThumbPath'Config conf id'APIGallery mediaId'APIGallery page_index image_type
			page_thumb_exist <- liftIO $ doesFileExist page_thumb_path
			if page_thumb_exist then next
			else do
				page_thumb_url <- mkPageThumbUrl mediaId'APIGallery page_index image_type
				fmap (Download page_thumb_url page_thumb_path :) next
			where
			image_type = type'ImageSpec page_image_spec
			next = my_list (page_index' + 1) rest
		my_list _ _ = pure []

runMainOption :: (MonadMask m, MonadBaseControl IO m, MonadLoggerIO m) => MainOption -> m ()
runMainOption (MainOptionDownload {..}) = run_download galleryIdListing'MainOptionDownload
	where
	run_download (GIDListingList {..}) = do
		mgr <- liftIO $ newManager tlsManagerSettings
		S.withStreamMapM
			(unrefine numThreads'MainOptionDownload)
			(runDownload mgr)
			(S.for (S.each galleryIdList'GIDListingList) (fetchGallery conf mgr))
			S.effects
	run_download GIDListingAll = do
		latest_gid <- unrefine <$> getLatestGalleryId
		$logInfo $ "Latest gallery: " <> T.pack (show $ latest_gid)
		gids <- traverse refineThrow $ enumFromThenTo (latest_gid) (latest_gid - 1) 1
		run_download (GIDListingList gids)
	conf = initConfig

main :: IO ()
main = do
	run_options <- execParser programOptionsParser
	runStdoutLoggingT $ runMainOption run_options
