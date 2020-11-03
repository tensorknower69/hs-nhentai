{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Data.NHentai.API.Gallery
where

import Data.Attoparsec.Number
import Control.Applicative
import Control.Error
import Control.Lens
import Data.Scientific
import Control.Monad.Catch
import Data.Aeson
import Data.NHentai.Internal.Utils
import Data.NHentai.Types
import Data.Text.Lens
import Refined
import Text.URI (URI)
import Text.URI hiding (URI(..))
import Text.URI.Lens
import Text.URI.QQ
import qualified Data.Text as T

mkGalleryApiUrl :: MonadThrow m => GalleryID -> m URI
mkGalleryApiUrl gid = do
	let prefix = [uri|https://nhentai.net/api/gallery|]
	gid_path_piece <- mkPathPiece (show (unrefine gid) ^. packed)
	pure $ prefix & uriPath %~ (<> [gid_path_piece])

toTagType :: [Char] -> Maybe TagType
toTagType str = readMay (capitalize str <> "Tag")

newtype APITagType = APITagType { unAPITagType :: TagType } deriving (Show, Eq)

instance FromJSON APITagType where
	parseJSON = withText "APITagType" $ \v -> do
		let v' = T.unpack v
		case toTagType v' of
			Just j -> pure $ APITagType j
			Nothing -> fail $ "unknown tag type: " <> show v'

newtype APIImageType = APIImageType { unAPIImageType :: ImageType } deriving (Show, Eq)

instance FromJSON APIImageType where
	parseJSON = withText "APIImageType" $ \case
		"j" -> pure $ APIImageType JPG
		"p" -> pure $ APIImageType PNG
		"g" -> pure $ APIImageType GIF
		c -> fail $ "unknown image type: " <> show c

newtype APIImageSpec = APIImageSpec { unAPIImageSpec :: ImageSpec } deriving (Show, Eq)

instance FromJSON APIImageSpec where
	parseJSON = withObject "APIImageSpec" $ \v -> do
		APIImageSpec <$> (ImageSpec
			<$> (unAPIImageType <$> v .: "t")
			<*> (v .: "w" >>= refineFail)
			<*> (v .: "h" >>= refineFail))

data APITag
	= APITag
		{ id'APITag :: TagID
		, type'APITag :: TagType
		, name'APITag :: T.Text
		, url'APITag :: T.Text
		, count'APITag :: Refined NonNegative Int
		}
	deriving (Show, Eq)

instance FromJSON APITag where
	parseJSON = withObject "APITag" $ \v -> APITag
		<$> (v .: "id")
		<*> (unAPITagType <$> v .: "type")
		<*> v .: "name"
		<*> v .: "url"
		<*> v .: "count"

data APIGallery
	= APIGallery
		{ id'APIGallery :: GalleryID
		, mediaId'APIGallery :: MediaID
		, titleEnglish'APIGallery :: T.Text
		, titleJapanese'APIGallery :: Maybe T.Text
		, titlePretty'APIGallery :: T.Text
		, pages'APIGallery :: [ImageSpec]
		, cover'APIGallery :: ImageSpec
		, thumbnail'APIGallery :: ImageSpec
		, scanlator'APIGallery :: T.Text
		, upload_date'APIGallery :: Integer
		, tags'APIGallery :: [APITag]
		, numPages'APIGallery :: PageIdx
		, numFavorites'APIGallery :: Refined NonNegative Int
		}
	deriving (Show, Eq)

getPageThumbUrl :: MonadThrow m => APIGallery -> PageIdx -> m URI
getPageThumbUrl g pid = do
	let p = (pages'APIGallery g) !! (unrefine pid - 1)
	let prefix = [uri|https://t.nhentai.net/galleries|]
	let mid = mediaId'APIGallery g
	mid_path_piece <- mkPathPiece (show (unrefine mid) ^. packed)
	image_path_piece <- mkPathPiece (show (unrefine pid) ^. packed <> "t." <> imageTypeExtension (type'ImageSpec p) ^. packed)
	pure $ prefix & uriPath %~ (<> [mid_path_piece, image_path_piece])

instance FromJSON APIGallery where
	parseJSON = withObject "APIGallery" $ \v -> do
		APIGallery
			<$> ( v .: "id" >>= \case
				Number x -> case floatingOrInteger x of
					Left f -> fail "is float"
					Right i -> leftFail . refineThrow $ i
				String y -> leftFail . refineThrow . read $ y ^. unpacked
				_ -> fail "unable to parse gallery id"
			)
			<*>  ( v .: "media_id" >>= \case
				Number x -> case floatingOrInteger x of
					Left f -> fail "is float"
					Right i -> leftFail . refineThrow $ i
				String y -> leftFail . refineThrow . read $ y ^. unpacked
				_ -> fail "unable to parse media id"
			)
			<*> (v .: "title" >>= (.: "english"))
			<*> (v .: "title" >>= (.: "japanese"))
			<*> (v .: "title" >>= (.: "pretty"))
			<*> (fmap unAPIImageSpec <$> (v .: "images" >>= (.: "pages")))
			<*> (unAPIImageSpec <$> (v .: "images" >>= (.: "cover")))
			<*> (unAPIImageSpec <$> (v .: "images" >>= (.: "thumbnail")))
			<*> v .: "scanlator"
			<*> v .: "upload_date"
			<*> v .: "tags"
			<*> v .: "num_pages"
			<*> v .: "num_favorites"
