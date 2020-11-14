{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.NHentai.API.Gallery where

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

-- e.g. https://i.nhentai.net/galleries/900513/2.00 https://nhentai.net/api/gallery/155974
-- e.g. https://nhentai.net/g/155844/24/
newtype APIImageType = APIImageType { unAPIImageType :: Either String ImageType } deriving (Show, Eq)

instance FromJSON APIImageType where
	parseJSON = withText "APIImageType" $ \string_packed -> do
		let string = T.unpack string_packed
		case headMay string of
			Nothing -> fail $ "image type string is empty"
			Just ch -> case ch ^? extensionChar of
				Nothing -> pure . APIImageType . Left $ string
				Just image_type -> pure . APIImageType . Right $ image_type

newtype APIImageSpec = APIImageSpec { unAPIImageSpec :: ImageSpec } deriving (Show, Eq)

instance FromJSON APIImageSpec where
	parseJSON = withObject "APIImageSpec" $ \v -> do
		APIImageSpec <$> (ImageSpec
			<$> (unAPIImageType <$> v .: "t")
			<*> (v .: "w" >>= refineFail)
			<*> (v .: "h" >>= refineFail))

data APITag
	= APITag
		{ _tagId :: TagID
		, _tagType :: TagType
		, _tagName :: T.Text
		, _tagUrl :: T.Text
		, _tagCount :: Refined NonNegative Int
		}
	deriving (Show, Eq)

makeLenses ''APITag

instance FromJSON APITag where
	parseJSON = withObject "APITag" $ \v -> APITag
		<$> (v .: "id")
		<*> (unAPITagType <$> v .: "type")
		<*> v .: "name"
		<*> v .: "url"
		<*> v .: "count"

data APIGallery
	= APIGallery
		{ _apiGalleryId :: GalleryID
		, _apiMediaId :: MediaID
		, _titleEnglish :: T.Text
		, _titleJapanese :: Maybe T.Text
		, _titlePretty :: T.Text
		, _pages :: [ImageSpec]
		, _cover :: ImageSpec
		, _thumbnail :: ImageSpec
		, _scanlator :: T.Text
		, _uploadDate :: UTCTime
		, _tags :: [APITag]
		, _numPages :: PageIndex
		, _numFavorites :: Refined NonNegative Int
		}
	deriving (Show, Eq)

makeLenses ''APIGallery

instance HasGalleryID APIGallery where
	galleryId = apiGalleryId

instance HasMediaID APIGallery where
	mediaId = apiMediaId

mkPageThumbnailUrl :: MonadThrow m => MediaID -> PageIndex -> ImageType -> m URI
mkPageThumbnailUrl mid pid imgtype = do
	mid_pp <- mkPathPiece (show (unrefine mid) ^. packed)
	img_pp <- mkPathPiece (show (unrefine pid) ^. packed <> "t." <> (extension # imgtype) ^. packed)
	pure $ prefix & uriPath %~ (<> [mid_pp, img_pp])
	where
	prefix = [uri|https://t.nhentai.net/galleries|]

mkPageImageUrl :: MonadThrow m => MediaID -> PageIndex -> ImageType -> m URI
mkPageImageUrl mid pid imgtype = do
	mid_pp <- mkPathPiece (show (unrefine mid) ^. packed)
	img_pp <- mkPathPiece (show (unrefine pid) ^. packed <> "." <> (extension # imgtype) ^. packed)
	pure $ prefix & uriPath %~ (<> [mid_pp, img_pp])
	where
	prefix = [uri|https://i.nhentai.net/galleries|]

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
