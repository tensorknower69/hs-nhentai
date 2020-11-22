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

import Control.Lens
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Trans.Control
import Data.Aeson hiding (json)
import Data.List
import Data.Maybe
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
import Streaming (Of)
import System.Directory
import System.FilePath
import System.IO
import Text.HTML.Scalpel.Core
import Text.URI
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BL
import qualified Data.List.NonEmpty as L
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
	-> S.Stream (Of Download) m ()
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
					let downloads = catMaybes <$> sequenceA
						[ mk_download
							(cfg ^. downloadPageThumbnailFlag)
							((cfg ^. pageThumbPathMaker) (g ^. galleryId) (g ^. mediaId) pageidx imgtype)
							(mkPageThumbnailUri (g ^. mediaId) pageidx imgtype)
						, mk_download
							(cfg ^. downloadPageImageFlag)
							((cfg ^. pageImagePathMaker) (g ^. galleryId) (g ^. mediaId) pageidx imgtype)
							(mkPageImageUri (g ^. mediaId) pageidx imgtype)
						]
					liftA2 (<>) downloads loop_rest

			where
			mk_download my_flag path m_uri = do
				if my_flag then do
					uri <- m_uri
					pure . Just $ Download uri path
				else do
					pure Nothing
			loop_rest = do
				pageidx' <- refineThrow $ unrefine pageidx + 1
				loop pageidx' rest
		loop _ [] = pure []

runMainOptions :: (MonadMask m, MonadBaseControl IO m, MonadLoggerIO m) => MainOptions -> m ()
runMainOptions (MainOptionsDownload {..}) = do
	run_main_options_download galleryIdListing'MainOptionsDownload
	where
	mk_download_config mgr = Config
		mgr
		downloadWarningOptions'MainOptionsDownload
		outputConfig'MainOptionsDownload
		downloadOptions'MainOptionsDownload

	run_main_options_download (GIDListingList {..}) = do
		mgr <- newTlsManager
		let cfg = mk_download_config mgr

		download_gids cfg (galleryIdList'GIDListingList)

	run_main_options_download GIDListingAll = do
		mgr <- newTlsManager
		let cfg = mk_download_config mgr

		unref_latest_gid <- unrefine <$> getLatestGalleryId mgr
		$logInfo $ "Latest gallery: " <> T.pack (show $ unref_latest_gid)

		let unref_gid_list = enumFromThenTo (unref_latest_gid) (unref_latest_gid - 1) 1
		(unref_gid_list & traversed refineThrow & over mapped L.nonEmpty) >>= \case
			Nothing -> throwM NHentaiNoGalleryException
			Just gids -> download_gids cfg gids

	download_gids cfg gids = do
		S.withStreamMapM
			(unrefine numThreads'MainOptionsDownload)
			(runDownload cfg)
			(S.for (S.each $ L.toList gids) (fetchGallery cfg))
			S.effects

		$logInfo $ "Done downloading all galleries: " <> T.pack (intercalate " " . fmap (show . unrefine) $ L.toList gids)

runMainOptions MainOptionsVersion = do
	liftIO $ putStrLn "0.1.0.1"

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
	let filtered = filterLogger (\_ level -> logLevel'ProgramOptions options <= level) $ runMainOptions (mainOptions'ProgramOptions options)
	runLoggingT filtered $ \loc source level logstr -> do
		let lvl_name = toLogStr $ drop 5 (show level)
		t <- getCurrentTime
		let line = toLogStr (iso8601Show t) <> ": " <> lvl_name <> ": " <> toLogStr logstr <> "\n"
		BSC.hPutStr stderr $ fromLogStr line
