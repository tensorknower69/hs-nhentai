{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Data.NHentai.Scraper.HomePage
( HomePage(..)
, homePagePagination
, popularGalleries
, recentGalleries

, homePageScraper
, galleryScraper

, mkHomePageUri
)
where

import Control.Applicative
import Control.Lens
import Control.Monad.Catch
import Data.List
import Data.List.NonEmpty
import Data.NHentai.Scraper.Types
import Data.NHentai.Types
import Data.Text.Lens
import Refined hiding (NonEmpty)
import Text.HTML.Scalpel.Core as Scalpel
import Text.StringLike hiding (empty)
import Text.URI (URI, QueryParam(..), mkQueryValue)
import Text.URI.Lens
import Text.URI.QQ

data HomePage
  = HomePage
    { _homePagePagination :: Pagination
    , _popularGalleries :: [ScraperGallery]
    , _recentGalleries :: NonEmpty ScraperGallery
    }
  deriving (Show, Eq)

makeLenses ''HomePage

instance HasPagination HomePage where
  pagination = homePagePagination

homePageScraper :: (Show str, StringLike str) => Scraper str HomePage
homePageScraper = do
  -- recent must be scrapped first
  recent' <- containerScraper []
  case nonEmpty recent' of
    Nothing -> fail "no recent galleries"
    Just recent -> do
      pagin <- chroot ("section" @: [hasClass "pagination"]) paginationScraper
      popular <- containerScraper ["index-popular"] <|> pure []
      pure $ HomePage pagin popular recent
  where
  containerScraper index_classes = chroots ("div" @: ["class" @= intercalate " " (["container", "index-container"] <> index_classes)] // "div" @: [hasClass "gallery"]) galleryScraper

mkHomePageUri :: MonadThrow m => PageIndex -> m URI
mkHomePageUri page = do
  page_query_value <- mkQueryValue $ show (unrefine page) ^. packed
  pure $ prefix & uriQuery .~ [ QueryParam [queryKey|page|] page_query_value ]
  where
  prefix = [uri|https://nhentai.net|]
