{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.NHentai.Scraper.Types
( Pagination(..)
, HasPagination(..)

, ScraperGallery(..)
, scraperGalleryId
, scraperMediaId
, dataTags
, caption
, coverImageSpec

, galleryScraper
, paginationScraper
)
where

import Control.Applicative
import Control.Error
import Control.Lens
import Control.Monad
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
    { _currentPage :: PageIndex
    , _lastPage :: PageIndex
    }
  deriving (Show, Eq)

makeClassy ''Pagination

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
    { _scraperGalleryId :: GalleryId
    , _scraperMediaId :: MediaId
    , _dataTags :: [TagId]
    , _caption :: T.Text
    , _coverImageSpec :: ImageSpec
    }
  deriving (Show, Eq)

makeLenses ''ScraperGallery

instance HasTitle ScraperGallery where
  title = caption

galleryScraper :: (Show str, StringLike str, Monad m) => ScraperT str m ScraperGallery
galleryScraper = do
  gid <- attr "href" ("a" @: [hasClass "cover"]) >>= extractGid
  tag_ids <- attr "data-tags" anySelector >>= extractDataTags
  capt <- castString <$> Scalpel.text ("div" @: [hasClass "caption"])
  data_src <- castString <$> (attr "data-src" "img") >>= justZ . mkURI
  mid <- data_src ^. uriPath ^? ix 1 . unRText . unpacked ^. to (justZ >=> readZ >=> refineFail)
  eitherimgtype <- data_src ^. uriPath ^? ix 2 . unRText . unpacked . prefixed "thumb." . to extensionToImageTypeEither ^. to justZ
  (w, h)  <- traverseOf each scrapDimension ("width", "height")
  pure $ ScraperGallery gid mid tag_ids capt (ImageSpec eitherimgtype w h)
  where
  extensionToImageTypeEither my_str = maybe (Left my_str) Right (my_str ^? extension)
  scrapDimension name = (castString <$> attr name "img") >>= readZ >>= refineFail
  extractDataTags x = traverse (readZ >=> refineFail) (splitOn " " (castString x))
  -- e.g. /g/177013/
  extractGid x = mkURI (castString x) ^? _Just . uriPath . ix 1 . unRText . from packed ^. to (justZ >=> readZ >=> refineFail)
