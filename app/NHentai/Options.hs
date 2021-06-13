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
    <> help "Download page thumbnails"
    )
  download_page_image_parser = switch
    ( short 'I'
    <> long "images"
    <> help "Download page images"
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
    ( long "warn-min-size"
    <> metavar "NUM_BYTES"
    <> help "Set size of content warning threshold"
    )
  warn_most_duration_parser = fmap (realToFrac . unrefine) $ option (refineReadM @Double @NonNegative)
    ( long "warn-min-duration"
    <> metavar "DURATION"
    <> help "Set download duration warning threshold"
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
    ( long "group-1000"
    <> help "Use another directory format, instead of \\gid -> dest_dir/<gid>/, the directory format will become \\gid -> dest_dir/<gid // 1000>/<gid>. Effectively grouping 1000 galleries into a each directory"
    )
  output_dir_parser = strOption
    ( short 'o'
    <> long "output"
    <> metavar "FILE"
    <> value "galleries"
    <> showDefault
    <> help "Set the output directory"
    )

data GidInputOption
  = GidInputOptionSingle
    { galleryId'GidInputOptionSingle :: GalleryId
    }
  | GidInputOptionListFile
    { filePath'GidInputOptionListFile :: FilePath
    }
  deriving (Show, Eq)

gidInputOptionParser :: Parser GidInputOption
gidInputOptionParser = single_parser <|> list_file_parser
  where
  single_parser = GidInputOptionSingle
    <$> option refineReadM
      ( short 'g'
      <> long "gallery"
      <> metavar "NUM"
      <> help "Specify a single gallery id"
      )
  list_file_parser = GidInputOptionListFile
    <$> strOption
      ( short 'f'
      <> long "include-from"
      <> metavar "FILE"
      <> help "Reading gallery ids from specified file. Skips invalid lines"
      )

data MainOptions
  = MainOptionsDownload
    { gidInputOption'MainOptionsDownload :: GidInputOption
    , numThreads'MainOptionsDownload :: Refined Positive Int
    , outputConfig'MainOptionsDownload :: OutputConfig
    , downloadWarningOptions'MainOptionsDownload :: DownloadWarningOptions
    , downloadOptions'MainOptionsDownload :: DownloadOptions
    }
  | MainOptionsVersion
  | MainOptionsLatestGid

mainOptionsParser :: Parser MainOptions
mainOptionsParser = subparser
  ( main_download_command
  <> main_version_command
  <> main_latest_gid_command
  )
  where
  main_download_command = command "download" $
    info (main_download_option <**> helper)
      ( fullDesc
      <> progDesc "Download galleries"
      )

  main_download_option = MainOptionsDownload
    <$> gidInputOptionParser
    <*> num_threads_parser
    <*> outputConfigParser
    <*> downloadWarningOptionsParser
    <*> downloadOptionsParser
    where
    num_threads_parser = option refineReadM
      ( short 't'
      <> long "threads"
      <> metavar "NUM"
      <> value $$(refineTH @Positive @Int 1)
      <> showDefault
      <> help "Set the number of threads used in downloading pages concurrently"
      )

  main_version_command = command "version" $ do
    info (pure MainOptionsVersion <**> helper)
      ( fullDesc
      <> progDesc "Print version"
      )

  main_latest_gid_command = command "latest-gid" $ do
    info (pure MainOptionsLatestGid <**> helper)
      ( fullDesc
      <> progDesc "Print the latest gallery's id"
      )

data ProgramOptions
  = ProgramOptions
    { maybeLogLevel'ProgramOptions :: Maybe LogLevel
    , mainOptions'ProgramOptions :: MainOptions
    }

programOptionsParser :: Parser ProgramOptions
programOptionsParser = ProgramOptions <$> maybe_log_level_parser <*> mainOptionsParser
  where
  maybe_log_level_parser :: Parser (Maybe LogLevel)
  maybe_log_level_parser = (Just <$> f) <|> pure Nothing
    where
    f = option logLevelReadM
      ( short 'l'
      <> long "log-level"
      <> metavar "LEVEL"
      <> help "Set log level (default disables logging). Prints possible inputs on error"
      )
