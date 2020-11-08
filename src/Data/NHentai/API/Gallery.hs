{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Data.NHentai.API.Gallery
( mkGalleryApiUrl
, mkPageThumbUrl

, APIGalleryResult(..)
, APIGallery(..)
, APITag(..)
)
where

import Control.Applicative
import Control.Error
import Control.Lens
import Control.Monad.Catch
import Data.Aeson
import Data.Aeson.Types
import Data.NHentai.Internal.Utils
import Data.NHentai.Types
import Data.Scientific
import Data.Text.Lens
import Data.Time.Clock
import Refined
import Text.URI (URI)
import Text.URI hiding (URI(..))
import Text.URI.Lens
import Text.URI.QQ
import qualified Data.Text as T

mkGalleryApiUrl :: MonadThrow m => GalleryID -> m URI
mkGalleryApiUrl gid = do
	gid_path_piece <- mkPathPiece (show (unrefine gid) ^. packed)
	pure $ prefix & uriPath %~ (<> [gid_path_piece])
	where
	prefix = [uri|https://nhentai.net/api/gallery|]

toTagType :: String -> Maybe TagType
toTagType str = readMay (capitalize str <> "Tag")

newtype APITagType = APITagType { unAPITagType :: TagType } deriving (Show, Eq)

instance FromJSON APITagType where
	parseJSON = withText "APITagType" $ \v -> do
		let v' = T.unpack v
		case toTagType v' of
			Just j -> pure $ APITagType j
			Nothing -> fail $ "unknown tag type: " <> show v'

newtype APIImageType = APIImageType { unAPIImageType :: Maybe ImageType } deriving (Show, Eq)

instance FromJSON APIImageType where
	parseJSON = withText "APIImageType" $ \r -> do
		case headMay (T.unpack r) of
			Nothing -> fail $ "image type string is empty"
			Just c -> case charToImageType c of
				Nothing -> fail $ "unknown image type: " <> show c
				Just t -> pure $ APIImageType t

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
		, uploadDate'APIGallery :: UTCTime
		, tags'APIGallery :: [APITag]
		, numPages'APIGallery :: PageIndex
		, numFavorites'APIGallery :: Refined NonNegative Int
		}
	deriving (Show, Eq)

mkPageThumbUrl :: MonadThrow m => MediaID -> PageIndex -> ImageType -> m URI
mkPageThumbUrl mid pid image_type = do
	mid_path_piece <- mkPathPiece (show (unrefine mid) ^. packed)
	image_path_piece <- mkPathPiece (show (unrefine pid) ^. packed <> "t." <> imageTypeToExtension image_type ^. packed)
	pure $ prefix & uriPath %~ (<> [mid_path_piece, image_path_piece])
	where
	prefix = [uri|https://t.nhentai.net/galleries|]

intOrString :: (Read a, Integral a) => Value -> Parser a
intOrString (Number i) = case floatingOrInteger @Float i of
	Left _ -> fail "is float"
	Right a -> pure a
intOrString (String x) = readZ (x ^. unpacked)
intOrString _ = fail "neither a String or a Number Int"

data APIGalleryResult
	= APIGalleryResultSuccess APIGallery
	| APIGalleryResultError T.Text
	deriving (Show, Eq)

instance FromJSON APIGalleryResult where
	parseJSON = withObject "APIGalleryResult" $ \v -> error_parser v <|> api_gallery_parser v
		where
		error_parser v = APIGalleryResultError
			<$> (v .: "error")
		api_gallery_parser v = APIGalleryResultSuccess <$>
			( APIGallery
			<$> (v .: "id" >>= intOrString >>= refineFail)
			<*> (v .: "media_id" >>= intOrString >>= refineFail)
			<*> (v .: "title" >>= (.: "english"))
			<*> (v .: "title" >>= (.: "japanese"))
			<*> (v .: "title" >>= (.: "pretty"))
			<*> (fmap unAPIImageSpec <$> (v .: "images" >>= (.: "pages")))
			<*> (unAPIImageSpec <$> (v .: "images" >>= (.: "cover")))
			<*> (unAPIImageSpec <$> (v .: "images" >>= (.: "thumbnail")))
			<*> v .: "scanlator"
			<*> (secondsToUTCTime <$> (v .: "upload_date"))
			<*> v .: "tags"
			<*> v .: "num_pages"
			<*> v .: "num_favorites"
			)
