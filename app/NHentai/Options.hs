{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module NHentai.Options where

import Control.Lens
import Control.Monad.Logger
import Data.Int
import Data.NHentai.Types
import Data.Time.Clock.POSIX
import NHentai.Utils
import Options.Applicative
import Refined
import System.FilePath
import qualified Data.List.NonEmpty as L

data DownloadOptions
	= DownloadOptions
		{ _downloadPageThumbnailFlag :: Bool
		, _downloadPageImageFlag :: Bool
		}
	deriving (Show, Eq)

makeClassy ''DownloadOptions

downloadOptionsParser :: Parser DownloadOptions
downloadOptionsParser = DownloadOptions
	<$> download_page_thumbnail_parser
	<*> download_page_image_parser
	where
	download_page_thumbnail_parser = switch
		( short 'T'
		<> long "thumbnails"
		<> help "Download page thumbnails of a gallery"
		)
	download_page_image_parser = switch
		( short 'I'
		<> long "images"
		<> help "Download page images of a gallery"
		)

data DownloadWarningOptions
	= DownloadWarningOptions
		{ _downloadWarnLeastSize :: Maybe (Refined NonNegative Int64)
		, _downloadWarnMostDuration :: Maybe POSIXTime
		}
	deriving (Show, Eq)

makeClassy ''DownloadWarningOptions

downloadWarningOptionsParser :: Parser DownloadWarningOptions
downloadWarningOptionsParser = DownloadWarningOptions
	<$> ((Just <$> warn_least_size_parser) <|> pure Nothing)
	<*> ((Just <$> warn_most_duration_parser) <|> pure Nothing)
	where
	warn_least_size_parser = option refineReadM
		( long "warn-least-size"
		<> metavar "NUM_BYTES"
		<> help "Set downloaded content's size threshold before warning"
		)
	warn_most_duration_parser = fmap (realToFrac . unrefine) $ option (refineReadM @Double @NonNegative)
		( long "warn-most-duration"
		<> metavar "DURATION"
		<> help "Set download duration threshold before warning"
		)

data OutputConfig
	= OutputConfig
		{ _jsonPathMaker :: GalleryId -> FilePath
		, _pageThumbPathMaker :: GalleryId -> MediaId -> PageIndex -> ImageType -> FilePath
		, _pageImagePathMaker :: GalleryId -> MediaId -> PageIndex -> ImageType -> FilePath
		}

makeClassy ''OutputConfig

simpleOutputConfig :: (GalleryId -> FilePath) -> OutputConfig
simpleOutputConfig prefix = OutputConfig
	{ _jsonPathMaker = \gid -> prefix gid </> "gallery.json"
	, _pageThumbPathMaker = \gid _ pid img_type -> prefix gid </> (show (unrefine pid) <> "t." <> extension # img_type)
	, _pageImagePathMaker = \gid _ pid img_type -> prefix gid </> (show (unrefine pid) <> "." <> extension # img_type)
	}

-- |The directory name will be the same as the gallery id
mkDefaultOutputConfig :: FilePath -> OutputConfig
mkDefaultOutputConfig output_dir = simpleOutputConfig $ \gid -> let unref_gid = unrefine gid in output_dir </> show unref_gid

-- |I don't really know how to explain this in English but here are some examples:
-- 344013 -> 344/344013
-- 177013 -> 177/177013
-- 9502 -> 9/502
-- 34 -> 0/34
mkDefaultOutputConfig2 :: FilePath -> OutputConfig
mkDefaultOutputConfig2 output_dir = simpleOutputConfig $ \gid -> let unref_gid = unrefine gid in output_dir </> show (unref_gid `div` 1000) </> show unref_gid

outputConfigParser :: Parser OutputConfig
outputConfigParser = (mk_conf2_parser <|> mk_conf1_parser) <*> output_dir_parser
	where
	mk_conf1_parser = pure mkDefaultOutputConfig
	mk_conf2_parser = flag' mkDefaultOutputConfig2
		( short '2'
		<> long "output-config-2"
		<> help "Use another directory format, instead of gid -> dest_dir/<gid>/, the directory format will become gid -> dest_dir/<div gid 1000>/<gid>"
		)
	output_dir_parser = strOption
		( short 'o'
		<> long "output-dir"
		<> metavar "OUTPUT_DIR"
		<> value "galleries"
		<> showDefault
		<> help "Set the output directory"
		)

data GIDListing
	= GIDListingList
		{ galleryIdList'GIDListingList :: L.NonEmpty GalleryId
		}
	| GIDListingAll
	deriving (Show, Eq)

gidListingParser :: Parser GIDListing
gidListingParser = list_parser <|> all_parser
	where
	list_parser = GIDListingList
		<$> option (nonEmptyReadM refineReadM)
			( short 'g'
			<> long "gallery-ids"
			<> metavar "GALLERY_IDS"
			<> help "Set the list of gallery ids to be downloaded, e.g. '177013' or '177013,166013,2'"
			)
	all_parser = flag' GIDListingAll
		( short 'a'
		<> long "all"
		<> help "Download galleries from the latest one to the beginning"
		)

data MainOptions
	= MainOptionsDownload
		{ galleryIdListing'MainOptionsDownload :: GIDListing
		, numThreads'MainOptionsDownload :: Refined Positive Int
		, outputConfig'MainOptionsDownload :: OutputConfig
		, downloadWarningOptions'MainOptionsDownload :: DownloadWarningOptions
		, downloadOptions'MainOptionsDownload :: DownloadOptions
		}
	| MainOptionsVersion

mainOptionsParser :: Parser MainOptions
mainOptionsParser = subparser
	( main_download_command
	<> main_version_command
	)
	where
	main_download_command = command "download" $
		info (main_download_option <**> helper)
			( fullDesc
			<> progDesc "Download pages of galleries"
			)

	main_download_option = MainOptionsDownload
		<$> gidListingParser
		<*> num_threads_parser
		<*> outputConfigParser
		<*> downloadWarningOptionsParser
		<*> downloadOptionsParser
		where
		num_threads_parser = option refineReadM
			( short 't'
			<> long "num-threads"
			<> metavar "NUM_THREADS"
			<> value $$(refineTH @Positive @Int 1)
			<> showDefault
			<> help "Set the number of threads used in downloading pages"
			)

	main_version_command = command "version" $ do
		info (pure MainOptionsVersion <**> helper)
			( fullDesc
			<> progDesc "Print version"
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
		( short 'l'
		<> long "log-level"
		<> metavar "LOG_LEVEL"
		<> value LevelDebug
		<> showDefault
		<> help "Set log level. Prints possible inputs on error."
		)
