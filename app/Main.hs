{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Error
import Data.Aeson
import Data.NHentai.Scraper.HomePage
import Data.NHentai.Types
import Data.NHentai.API.Gallery
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Control.Monad
import Refined
import System.Directory
import System.FilePath
import Text.HTML.Scalpel hiding (Config)
import Text.URI
import qualified Data.ByteString.Lazy as BL

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
		, getGalleryPageThumbPath'Config :: GalleryID -> MediaID -> PageIdx -> ImageType -> FilePath
		}

initConfig :: Config
initConfig = Config
	{ getGalleryJsonPath'Config = \gid -> "galleries" </> show (unrefine gid) </> "api.json"
	, getGalleryPageThumbPath'Config = \gid _ pid img_type -> "galleries" </> show (unrefine gid) </> (show (unrefine pid) <> "." <> imageTypeExtension img_type)
	}

downloadGallery :: Manager -> Config -> GalleryID -> IO ()
downloadGallery mgr conf gid = do
	let json_path = getGalleryJsonPath'Config conf gid
	
	json_exist <- doesFileExist json_path

	unless (json_exist) $ do
		putStrLn "Downloading JSON."
		createDirectoryIfMissing True (takeDirectory json_path)
		json_url <- mkGalleryApiUrl gid
		req <- parseRequest (renderStr json_url)
		body <- responseBody <$> httpLbs req mgr
		print body
		BL.writeFile json_path body

	eitherDecodeFileStrict json_path >>= \case
		Left err -> fail err
		Right g -> do
			putStrLn "Downloading Stuff."
			let mid = mediaId'APIGallery g
			forM_ (zip [1..] (pages'APIGallery g)) $ \(pid',p) -> do
				pid <- refineThrow pid'
				let image_type = type'ImageSpec p
				let page_thumb_path = getGalleryPageThumbPath'Config conf gid mid pid image_type
				page_thumb_exist <- doesFileExist page_thumb_path
				unless (page_thumb_exist) $ do
					createDirectoryIfMissing True (takeDirectory page_thumb_path)
					page_thumb_url <- getPageThumbUrl g pid
					putStrLn $ renderStr page_thumb_url
					req2 <- parseRequest (renderStr page_thumb_url)
					body2 <- responseBody <$> httpLbs req2 mgr
					putStrLn page_thumb_path
					BL.writeFile page_thumb_path body2

main :: IO ()
main = do
	gid <- getRecentGalleryId
	print gid
	mgr <- newManager tlsManagerSettings
	downloadGallery mgr initConfig gid
