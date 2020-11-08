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
import Control.Monad.State
import Control.Monad.Trans.Control
import Data.Aeson hiding (json)
import Data.List
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

requestURI :: (MonadCatch m, MonadIO m) => Manager -> URI -> m BL.ByteString
requestURI mgr uri = do
	req <- parseRequest (renderStr uri)
	let req' = req
		{ responseTimeout = responseTimeoutNone
		}
	responseBody <$> liftIO (httpLbs req' mgr)

getLatestGalleryId :: (MonadCatch m, MonadIO m) => Manager -> m GalleryID
getLatestGalleryId mgr = do
	url <- mkHomePageUrl $$(refineTH 1)
	body <- requestURI mgr url
	case scrapeStringLike body homePageScraper of
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
		<> value "galleries"
		<> showDefault
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

download :: (MonadCatch m, MonadLoggerIO m) => Manager -> URI -> FilePath -> m ()
download mgr uri dest_path = do
	$logDebug $ "Downloading: " <> arrow_text <> "..."
	t <- liftIO getPOSIXTime
	body <- fix $ \loop -> do
		requestURI mgr uri `catch` \(e :: SomeException) -> do
			$logError $ "Redownloading, error when doing request: " <> render uri <> "\n- Error: " <> T.pack (show e)
			loop
	t' <- liftIO getPOSIXTime

	let dt = t' - t
	let body_length = BL.length body
	$logDebug $ "Downloaded: " <> arrow_text <> ", took " <> T.pack (show dt) <> ", byte length: " <> T.pack (show body_length)

	when (body_length <= least_size) $ do
		$logWarn $ "Downloaded content's size is too small: " <> T.pack (show body_length) <> " (<= " <> T.pack (show least_size) <> " bytes): " <> arrow_text <> ", the content may be invalid or not"
	when (dt > most_dt) $ do
		$logWarn $ "Downloading time took too long: " <> T.pack (show dt) <> " (>= " <> T.pack (show most_dt) <> "): " <> arrow_text

	liftIO $ createDirectoryIfMissing True (takeDirectory dest_path)
	liftIO $ BL.writeFile dest_path body
	where
	arrow_text = render uri <> " -> " <> T.pack dest_path
	least_size = 2000
	most_dt = 5.0

fetch :: (MonadCatch m, MonadLoggerIO m) => Manager -> URI -> FilePath -> m BL.ByteString
fetch mgr uri dest_path = do
	exist <- liftIO $ doesFileExist dest_path
	if exist then do
		$logDebug $ "Skipped downloading: " <> arrow_text <> ", file exists"
	else do
		download mgr uri dest_path
	liftIO $ BL.readFile dest_path
	where
	arrow_text = render uri <> " -> " <> T.pack dest_path

data Download = Download URI FilePath
	deriving (Show, Eq)

runDownload :: (MonadCatch m, MonadLoggerIO m) => Manager -> Download -> m ()
runDownload mgr (Download url dest_path) = download mgr url dest_path

data DownloaderState
	= DownloaderState
		{ -- TODO
		}
	deriving (Show, Eq)

initDownloaderState :: DownloaderState
initDownloaderState = DownloaderState

fetchGallery :: (MonadCatch m, MonadLoggerIO m, MonadState DownloaderState m) => OutputConfig -> Manager -> GalleryID -> Stream (Of Download) m ()
fetchGallery conf mgr gid = do
	gallery_api_url <- mkGalleryApiUrl gid
	$logInfo $ "Fetching gallery: " <> render gallery_api_url
	body <- fetch mgr gallery_api_url gallery_json_path
	case eitherDecode @APIGalleryResult body of
		Left error_string -> do
			$logError $ "Unable to decode JSON from gallery: " <> T.pack (show $ unrefine gid) <> "\n- Error: " <> T.pack error_string <> "\n- Body: " <> T.pack (show body)
			$logInfo $ "Redownloading " <> T.pack (show $ unrefine gid) <> "..."
			liftIO $ removeFile gallery_json_path
			fetchGallery conf mgr gid
		Right (APIGalleryResultError _) -> do
			$logWarn $ "Gallery is dead: " <> T.pack (show $ unrefine gid) <> ", skipping"
			pure ()
		Right (APIGalleryResultSuccess api_gallery) -> do
			downloads <- extractDownloads api_gallery
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
runMainOptions (MainOptionsDownload {..}) = do
	run_download galleryIdListing'MainOptionsDownload
	where
	run_download (GIDListingList {..}) = do
		mgr <- newTlsManager
		download_gids (galleryIdList'GIDListingList) mgr
	run_download GIDListingAll = do
		mgr <- newTlsManager
		latest_gid <- unrefine <$> getLatestGalleryId mgr
		$logInfo $ "Latest gallery: " <> T.pack (show $ latest_gid)
		gids <- traverse refineThrow $ enumFromThenTo (latest_gid) (latest_gid - 1) 1
		download_gids gids mgr
	download_gids gids mgr = do
		(flip evalStateT) initDownloaderState $ S.withStreamMapM
			(unrefine numThreads'MainOptionsDownload)
			(runDownload mgr)
			(S.for
				(S.each gids)
				(fetchGallery outputConfig'MainOptionsDownload mgr)
			)
			S.effects
		$logInfo $ "Done downloading all galleries: " <> T.pack (intercalate " " . fmap (show . unrefine) $ gids)

main :: IO ()
main = do
	options <- execParser $ info (programOptionsParser <**> helper)
		( fullDesc
		<> header "nhentai-cli - a command line interface for nhentai.net written in Haskell"
		)
	runStdoutLoggingT . filterLogger (\_ level -> logLevel'ProgramOptions options <= level) $ runMainOptions (mainOptions'ProgramOptions options)
