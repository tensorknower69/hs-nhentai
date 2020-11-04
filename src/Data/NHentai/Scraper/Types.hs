{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Data.NHentai.Scraper.Types
where

import Control.Applicative
import Control.Error
import Control.Lens
import Control.Monad
import Data.Char
import Data.List.Lens
import Data.List.Split
import Data.NHentai.Types
import Data.Text.Lens
import Refined
import Text.HTML.Scalpel.Core
import Text.HTML.Scalpel.Core as Scalpel
import Text.StringLike
import Text.URI (mkURI)
import Text.URI.Lens
import Text.URI.QQ
import qualified Data.Text as T

data Pagination
	= Pagination
		{ current'Pagination :: PageIndex
		, last'Pagination :: PageIndex
		}
	deriving (Show, Eq)

paginationScraper :: (Show str, StringLike str) => Scraper str Pagination
paginationScraper = Pagination <$> current <*> (last' <|> current)
	where
	current = get ["page", "current"]
	last' = get ["last"]
	get classes = attr "href" ("a" @: map hasClass classes) >>= extract
	-- e.g. "?page=1&sort=popular"
	extract href = mkURI (castString href) ^? _Just . uriQuery . queryParam [queryKey|page|] . unRText . from packed ^. to (justZ >=> readZ >=> refineFail)

data ScraperGallery
	= ScraperGallery
		{ galleryId'ScraperGallery :: GalleryID
		, mediaId'ScraperGallery :: MediaID
		, dataTags'ScraperGallery :: [TagID]
		, caption'ScraperGallery :: T.Text
		, coverImageSpec'ScraperGallery :: ImageSpec
		}
	deriving (Show, Eq)

galleryScraper :: (Show str, StringLike str, Monad m) => ScraperT str m ScraperGallery
galleryScraper = do
	gid <- attr "href" ("a" @: [hasClass "cover"]) >>= extractGid
	tag_ids <- attr "data-tags" anySelector >>= extractDataTags
	caption <- castString <$> Scalpel.text ("div" @: [hasClass "caption"])
	data_src <- castString <$> (attr "data-src" "img") >>= justZ . mkURI
	media_id <- data_src ^. uriPath ^? ix 1 . unRText . unpacked ^. to (justZ >=> readZ >=> refineFail)
	image_type <- data_src ^. uriPath ^? ix 2 . unRText . unpacked . prefixed "thumb." . to (map toUpper) ^. to (justZ >=> readZ)
	(width, height)  <- traverseOf each scrapDimension ("width", "height")
	pure $ ScraperGallery gid media_id tag_ids caption (ImageSpec image_type width height)
	where
	scrapDimension name = (castString <$> attr name "img") >>= readZ >>= refineFail
	extractDataTags x = traverse (readZ >=> refineFail) (splitOn " " (castString x))
	-- e.g. /g/177013/
	extractGid x = mkURI (castString x) ^? _Just . uriPath . ix 1 . unRText . from packed ^. to (justZ >=> readZ >=> refineFail)
