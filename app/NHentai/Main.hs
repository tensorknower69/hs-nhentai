{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Lens
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Trans.Control
import Data.Aeson hiding (json)
import Data.List
import Data.NHentai.API.Gallery
import Data.NHentai.Scraper.HomePage
import Data.NHentai.Scraper.Types
import Data.NHentai.Types
import NHentai.Options
import NHentai.Utils
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Options.Applicative
import Refined
import Streaming (Of)
import System.Directory
import System.FilePath
import Text.HTML.Scalpel.Core
import Text.URI
import qualified Data.ByteString.Lazy as BL
import qualified Data.List.NonEmpty as L
import qualified Data.Text as T
import qualified Streaming.Concurrent as S
import qualified Streaming.Prelude as S

class HasManager a where
	manager :: Lens' a Manager

instance HasManager Manager where
	manager = id

requestURI :: (MonadCatch m, MonadIO m, HasManager cfg) => cfg -> URI -> m BL.ByteString
requestURI cfg uri = do
	req <- parseRequest (renderStr uri)
	let req' = req
		{ responseTimeout = responseTimeoutNone
		}
	responseBody <$> liftIO (httpLbs req' (cfg ^. manager))

getLatestGalleryId :: (MonadCatch m, MonadIO m, HasManager cfg) => cfg -> m GalleryID
getLatestGalleryId cfg = do
	url <- mkHomePageUrl $$(refineTH 1)
	body <- requestURI cfg url
	case scrapeStringLike body homePageScraper of
		Nothing -> throwM $ ScalpelException body
		Just home_page -> pure $ home_page ^. recentGalleries . head1 . scraperGalleryId

data Config
	= Config
		{ _cfgDownloaderWarningOptions :: DownloaderWarningOptions
		, _cfgManager :: Manager
		, _cfgOutputConfig :: OutputConfig
		}

makeLenses ''Config

instance HasDownloaderWarningOptions Config where
	downloaderWarningOptions = cfgDownloaderWarningOptions

instance HasOutputConfig Config where
	outputConfig = cfgOutputConfig

instance HasManager Config where
	manager = cfgManager

download :: (MonadCatch m, MonadLoggerIO m, HasManager cfg, HasDownloaderWarningOptions cfg) => cfg -> URI -> FilePath -> m ()
download cfg uri dest_path = do
	$logDebug $ "Downloading: " <> arrow_text <> "..."

	(dt, body) <- withTimer $ fix $ \loop -> do
		requestURI (cfg ^. manager) uri `catch` \(e :: SomeException) -> do
			$logError $ "Redownloading, error when doing request: " <> render uri <> "\n- Error: " <> T.pack (show e)
			loop

	let body_length = BL.length body
	$logDebug $ "Downloaded: " <> arrow_text <> ", took " <> T.pack (show dt) <> ", byte length: " <> T.pack (show body_length)
	
	case cfg ^. downloaderWarnLeastSize of
		Nothing -> pure ()
		Just least_size -> when (body_length <= unrefine least_size) $ do
			$logWarn $ "Downloaded content's size is too small: " <> T.pack (show body_length) <> " (<= " <> T.pack (show least_size) <> " bytes): " <> arrow_text <> ", the content may be invalid or not"

	case cfg ^. downloaderWarnMostDuration of
		Nothing -> pure ()
		Just most_dt -> when (dt > most_dt) $ do
			$logWarn $ "Downloading time took too long: " <> T.pack (show dt) <> " (>= " <> T.pack (show most_dt) <> "): " <> arrow_text

	liftIO $ do
		createDirectoryIfMissing True (takeDirectory dest_path)
		BL.writeFile dest_path body
	where
	arrow_text = render uri <> " -> " <> T.pack dest_path

data Download = Download URI FilePath
	deriving (Show, Eq)

runDownload :: (MonadCatch m, MonadLoggerIO m, HasManager cfg, HasDownloaderWarningOptions cfg) => cfg -> Download -> m ()
runDownload cfg (Download url dest_path) = download cfg url dest_path

fetchGallery :: (MonadCatch m, MonadLoggerIO m, HasOutputConfig cfg, HasManager cfg, HasDownloaderWarningOptions cfg) => cfg -> GalleryID -> S.Stream (Of Download) m ()
fetchGallery cfg gid = do
	gallery_api_url <- mkGalleryApiUrl gid
	$logInfo $ "Fetching gallery: " <> render gallery_api_url
	body_file_exist <- liftIO $ doesFileExist gallery_json_path
	if body_file_exist then do
		let arrow_text = render gallery_api_url <> " -> " <> T.pack gallery_json_path
		$logDebug $ "Skipped downloading: " <> arrow_text <> ", file exists"
	else do
		download cfg gallery_api_url gallery_json_path
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
		Right (APIGalleryResultSuccess g) -> do
			downloads <- extractDownloads g
			S.each downloads
	where
	gallery_json_path = (cfg ^. jsonPathMaker) gid
	extractDownloads g = loop $$(refineTH 1) (g ^. pages)
		where
		loop pageidx (imgspec : rest) = do
			case imgspec ^. eitherImageType of
				Left string -> do
					$logWarn $ "Image type is "
						<> T.pack (show string)
						<> ": Gallery: "
						<> T.pack (show $ unrefine $ g ^. galleryId)
						<> ", page: "
						<> T.pack (show $ unrefine pageidx)
						<> ", the image is probably invalid, skipping"
					loop_rest
				Right imgtype -> do
					let page_thumb_path = (cfg ^. pageThumbPathMaker) (g ^. galleryId) (g ^. mediaId) pageidx imgtype
					page_thumb_exist <- liftIO $ doesFileExist page_thumb_path
					if page_thumb_exist then do
						loop_rest
					else do
						page_thumb_url <- mkPageThumbUrl (g ^. mediaId) pageidx imgtype
						cons (Download page_thumb_url page_thumb_path) <$> loop_rest

			where
			loop_rest = do
				pageidx' <- refineThrow $ unrefine pageidx + 1
				loop pageidx' rest
		loop _ [] = pure []

runMainOptions :: (MonadMask m, MonadBaseControl IO m, MonadLoggerIO m) => MainOptions -> m ()
runMainOptions (MainOptionsDownload {..}) = do
	run_main_options_download galleryIdListing'MainOptionsDownload
	where
	mk_downloader_config mgr = Config
		downloaderWarningOptions'MainOptionsDownload
		mgr
		outputConfig'MainOptionsDownload

	run_main_options_download (GIDListingList {..}) = do
		mgr <- newTlsManager
		let cfg = mk_downloader_config mgr

		download_gids cfg (galleryIdList'GIDListingList)

	run_main_options_download GIDListingAll = do
		mgr <- newTlsManager
		let cfg = mk_downloader_config mgr

		unref_latest_gid <- unrefine <$> getLatestGalleryId mgr
		$logInfo $ "Latest gallery: " <> T.pack (show $ unref_latest_gid)

		(enumFromThenTo (unref_latest_gid) (unref_latest_gid - 1) 1 & traversed refineThrow & over mapped L.nonEmpty) >>= \case
			Nothing -> throwM NHentaiNoGalleryException
			Just gids -> download_gids cfg gids

	download_gids cfg gids = do
		S.withStreamMapM
			(unrefine numThreads'MainOptionsDownload)
			(runDownload cfg)
			(S.for
				(S.each $ L.toList gids)
				(fetchGallery cfg)
			)
			S.effects
		$logInfo $ "Done downloading all galleries: " <> T.pack (intercalate " " . fmap (show . unrefine) $ L.toList gids)

main :: IO ()
main = do
	options <- execParser $ info (programOptionsParser <**> helper)
		( fullDesc
		<> progDesc "A scraper/downloader for nhentai.net"
		)
	runStdoutLoggingT . filterLogger (\_ level -> logLevel'ProgramOptions options <= level) $ runMainOptions (mainOptions'ProgramOptions options)
