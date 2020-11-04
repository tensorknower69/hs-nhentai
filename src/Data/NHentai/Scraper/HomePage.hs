{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.NHentai.Scraper.HomePage
( mkHomePageUrl

, HomePage(..)
, homePageScraper
)
where

import Control.Applicative
import Control.Lens
import Control.Monad.Catch
import Data.List
import Data.NHentai.Scraper.Types
import Data.NHentai.Types
import Data.Text.Lens
import Refined
import Text.HTML.Scalpel.Core as Scalpel
import Text.StringLike hiding (empty)
import Text.URI (URI, QueryParam(..), mkQueryValue)
import Text.URI.Lens
import Text.URI.QQ

data HomePage
	= HomePage
		{ pagination'HomePage :: Pagination
		, popularGalleries'HomePage :: [ScraperGallery]
		, recentGalleries'HomePage :: [ScraperGallery]
		}
	deriving (Show, Eq)

homePageScraper :: (Show str, StringLike str) => Scraper str (Maybe HomePage)
homePageScraper = do
	-- recent must be scrapped first
	recent <- containerScraper []
	if null recent then
		pure Nothing
	else do
		pagination <- chroot ("section" @: [hasClass "pagination"]) paginationScraper
		popular <- containerScraper ["index-popular"] <|> pure []
		pure . Just $ HomePage pagination popular recent
	where
	containerScraper index_classes = chroots ("div" @: ["class" @= intercalate " " (["container", "index-container"] <> index_classes)] // "div" @: [hasClass "gallery"]) galleryScraper

mkHomePageUrl :: MonadThrow m => PageIndex -> m URI
mkHomePageUrl page = do
	let prefix = [uri|https://nhentai.net|]
	page_query_value <- mkQueryValue $ show (unrefine page) ^. packed
	pure $ prefix & uriQuery .~ [ QueryParam [queryKey|page|] page_query_value ]
