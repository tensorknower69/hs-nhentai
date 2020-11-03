{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Data.NHentai.API.Comment where

import Control.Lens
import Control.Monad.Catch
import Data.Aeson
import Data.NHentai.API.Gallery (mkGalleryApiUrl)
import Data.NHentai.Internal.Utils
import Data.NHentai.Types
import Refined
import Text.URI (URI)
import Text.URI hiding (URI(..))
import Text.URI.Lens
import Text.URI.QQ
import qualified Data.Text as T

mkCommentApiUrl :: MonadThrow m => GalleryID -> m URI
mkCommentApiUrl gid = do
	url <- mkGalleryApiUrl gid
	pure $ url & uriPath %~ (<> [[pathPiece|comments|]])

data APIPoster
	= APIPoster
		{ id'APIPoster :: PosterID
		, username'APIPoster :: T.Text
		, slug'APIPoster :: T.Text
		, avatarUrl'APIPoster :: URI
		, isSuperUser'APIPoster :: Bool
		, isStaff'APIPoster :: Bool
		}
	deriving (Show, Eq)

instance FromJSON APIPoster where
	parseJSON = withObject "APIPoster" $ \v -> APIPoster
		<$> (v .: "id" >>= refineFail)
		<*> v .: "username"
		<*> v .: "slug"
		<*> ((v .: "avatar_url") >>= leftFail . mkURI)
		<*> v .: "is_superuser"
		<*> v .: "is_staff"

data APIComment
	= APIComment
		{ id'APIComment :: CommentID
		, galleryId'APIComment :: GalleryID
		, poster'APIComment :: APIPoster
		, postDate'APIComment :: Integer
		, body'APIComment :: T.Text
		}
	deriving (Show, Eq)

instance FromJSON APIComment where
	parseJSON = withObject "APIComment" $ \v -> APIComment
		<$> (v .: "id" >>= refineFail)
		<*> (v .: "gallery_id" >>= refineFail)
		<*> v .: "poster"
		<*> v .: "post_date"
		<*> v .: "body"
