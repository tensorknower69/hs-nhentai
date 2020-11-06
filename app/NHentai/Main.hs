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
import Text.HTML.Scalpel
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

data OutputConfig
	= OutputConfig
		{ getGalleryJsonPath'OutputConfig :: GalleryID -> FilePath
		, getGalleryPageThumbPath'OutputConfig :: GalleryID -> MediaID -> PageIndex -> ImageType -> FilePath
		}

simpleOutputConfig :: (GalleryID -> FilePath) -> OutputConfig
simpleOutputConfig prefix = OutputConfig
	{ getGalleryJsonPath'OutputConfig = \gid -> prefix gid </> "gallery.json"
	, getGalleryPageThumbPath'OutputConfig = \gid _ pid img_type -> prefix gid </> (show (unrefine pid) <> "." <> imageTypeExtension img_type)
	}

mkDefaultOutputConfig :: FilePath -> OutputConfig
mkDefaultOutputConfig output_dir = simpleOutputConfig $ \gid -> let gid' = unrefine gid in output_dir </> show gid'

mkDefaultOutputConfig2 :: FilePath -> OutputConfig
mkDefaultOutputConfig2 output_dir = simpleOutputConfig $ \gid -> let gid' = unrefine gid in output_dir </> show (gid' `div` 1000) </> show gid'

outputConfigParser :: Parser OutputConfig
outputConfigParser = (mk_conf2_parser <|> mk_conf1_parser) <*> output_dir_parser
	where
	mk_conf1_parser = pure mkDefaultOutputConfig
	mk_conf2_parser = flag' mkDefaultOutputConfig2
		( short '2'
		<> long "output-config-2"
		<> help "Use another method of storing the output" -- TODO: write a better description
		)
	output_dir_parser = strOption
		( short 'o'
		<> long "output-dir"
		<> metavar "OUTPUT_DIR"
		<> help "Set the output directory"
		)

data GalleryIDListing
	= GIDListingList
		{ galleryIdList'GIDListingList :: [GalleryID]
		}
	| GIDListingAll
	deriving (Show, Eq)

gidListingParser :: Parser GalleryIDListing
gidListingParser = list_parser <|> all_parser
	where
	list_parser = GIDListingList
		<$> option (listReadM refineReadM)
			( short 'g'
			<> long "gallery-ids"
			<> metavar "GALLERY_IDS"
			<> help "List of ids fo galleries to be downloaded"
			)
	all_parser = flag' GIDListingAll
		( short 'a'
		<> long "all"
		<> help "Download galleries from the latest one to the beginning"
		)

data MainOptions
	= MainOptionsDownload
		{ galleryIdListing'MainOptionsDownload :: GalleryIDListing
		, numThreads'MainOptionsDownload :: Refined Positive Int
		, outputConfig'MainOptionsDownload :: OutputConfig
		}

mainOptionsParser :: Parser MainOptions
mainOptionsParser = subparser
	( main_download_command
	)
	where
	main_download_command = command "download" (info (main_download_option <**> helper) mempty)
	main_download_option = MainOptionsDownload
		<$> gidListingParser
		<*> num_threads_parser
		<*> outputConfigParser
		where
		num_threads_parser = option refineReadM
			( short 't'
			<> long "num-threads"
			<> metavar "NUM_THREADS"
			<> value $$(refineTH @Positive @Int 1)
			<> showDefault
			<> help "Set the number of threads used in download images"
			)

data ProgramOptions
	= ProgramOptions
		{ logLevel'ProgramOptions :: LogLevel
		, mainOptions'ProgramOptions :: MainOptions
		}

programOptionsParser :: Parser ProgramOptions
programOptionsParser = ProgramOptions <$> log_level_parser <*> mainOptionsParser
	where
	log_level_parser :: Parser LogLevel
	log_level_parser = option logLevelReadM
		( short 'v'
		<> long "log-level"
		<> metavar "LOG_LEVEL"
		<> value LevelDebug
		<> showDefault
		)

download :: (MonadThrow m, MonadLoggerIO m) => Manager -> URI -> FilePath -> m ()
download mgr uri dest_path = do
	req <- parseRequest (renderStr uri)
	$logDebug $ "Downloading: " <> arrow_str <> "..."
	t <- liftIO getPOSIXTime
	body <- responseBody <$> liftIO (httpLbs req mgr)
	t' <- liftIO getPOSIXTime
	let dt = t' - t
	$logDebug $ "Downloaded: " <> arrow_str <> ", took " <> T.pack (show dt) <> ", byte length: " <> T.pack (show $ BL.length body)
	liftIO $ createDirectoryIfMissing True (takeDirectory dest_path)
	liftIO $ BL.writeFile dest_path body
	where
	arrow_str = render uri <> " -> " <> T.pack dest_path

fetch :: (MonadThrow m, MonadLoggerIO m) => Manager -> URI -> FilePath -> m BL.ByteString
fetch mgr uri dest_path = do
	exist <- liftIO $ doesFileExist dest_path
	if exist then do
		$logDebug $ "Skipped downloading: " <> arrow_str <> ", file exists"
	else do
		download mgr uri dest_path
	liftIO $ BL.readFile dest_path
	where
	arrow_str = render uri <> " -> " <> T.pack dest_path

data Download = Download URI FilePath
	deriving (Show, Eq)

runDownload :: (MonadThrow m, MonadLoggerIO m) => Manager -> Download -> m ()
runDownload mgr (Download url dest_path) = download mgr url dest_path

fetchGallery :: (MonadThrow m, MonadLoggerIO m) => OutputConfig -> Manager -> GalleryID -> Stream (Of Download) m ()
fetchGallery conf mgr gid = do
	gallery_api_url <- mkGalleryApiUrl gid
	(eitherDecode @APIGallery <$> fetch mgr gallery_api_url gallery_json_path) >>= \case
		Left error_string -> throwM (AesonParseException error_string)
		Right json -> do
			downloads <- extractDownloads json
			S.each downloads
	where
	gallery_json_path = getGalleryJsonPath'OutputConfig conf gid
	extractDownloads (APIGallery {..}) = my_list 1 pages'APIGallery
		where
		my_list page_index' (page_image_spec : rest) = do
			page_index <- refineThrow page_index'
			let page_thumb_path = getGalleryPageThumbPath'OutputConfig conf id'APIGallery mediaId'APIGallery page_index image_type
			page_thumb_exist <- liftIO $ doesFileExist page_thumb_path
			if page_thumb_exist then next
			else do
				page_thumb_url <- mkPageThumbUrl mediaId'APIGallery page_index image_type
				fmap (Download page_thumb_url page_thumb_path :) next
			where
			image_type = type'ImageSpec page_image_spec
			next = my_list (page_index' + 1) rest
		my_list _ _ = pure []

runMainOptions :: (MonadMask m, MonadBaseControl IO m, MonadLoggerIO m) => MainOptions -> m ()
runMainOptions (MainOptionsDownload {..}) = run_download galleryIdListing'MainOptionsDownload
	where
	run_download (GIDListingList {..}) = do
		mgr <- liftIO $ newManager tlsManagerSettings
		S.withStreamMapM
			(unrefine numThreads'MainOptionsDownload)
			(runDownload mgr)
			(S.for (S.each galleryIdList'GIDListingList) (fetchGallery outputConfig'MainOptionsDownload mgr))
			S.effects
	run_download GIDListingAll = do
		latest_gid <- unrefine <$> getLatestGalleryId
		$logInfo $ "Latest gallery: " <> T.pack (show $ latest_gid)
		gids <- traverse refineThrow $ enumFromThenTo (latest_gid) (latest_gid - 1) 1
		run_download (GIDListingList gids)

main :: IO ()
main = do
	options <- execParser $ info (programOptionsParser <**> helper)
		( fullDesc
		<> header "nhentai-cli - a command line interface for nhentai.net written in Haskell"
		)
	runStdoutLoggingT . filterLogger (\_ level -> logLevel'ProgramOptions options <= level) $ runMainOptions (mainOptions'ProgramOptions options)