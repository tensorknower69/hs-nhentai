{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.NHentai.API.Comment
( mkCommentApiUri
, APIPoster(..)
, posterId
, posterUsername
, posterSlug
, posterAvatarUri
, posterIsSuperUser
, posterIsStaff

, APIComment(..)
, commentId
, commentGalleryId
, commentPoster
, commentPostDate
, commentBody
)
where

import Control.Lens
import Control.Monad.Catch
import Data.Aeson
import Data.NHentai.API.Gallery (mkGalleryApiUri)
import Data.NHentai.Internal.Utils
import Data.NHentai.Types
import Data.Time.Clock
import Language.Haskell.TH.Syntax
import Refined
import Text.URI (URI)
import Text.URI.Lens
import Text.URI.QQ
import qualified Data.Text as T

mkCommentApiUri :: MonadThrow m => GalleryId -> m URI
mkCommentApiUri gid = do
  uri <- mkGalleryApiUri gid
  pure $ uri & uriPath %~ (<> [[pathPiece|comments|]])

data APIPoster
  = APIPoster
    { _posterId :: PosterId
    , _posterUsername :: T.Text
    , _posterSlug :: T.Text
    , _posterAvatarUri :: URI
    , _posterIsSuperUser :: Bool
    , _posterIsStaff :: Bool
    }
  deriving (Show, Eq)

makeLensesWith (classyRules & lensClass .~ const (Just (mkName "HasAPIPoster", mkName "apiPoster"))) ''APIPoster

instance FromJSON APIPoster where
  parseJSON = withObject "APIPoster" $ \v -> APIPoster
    <$> (v .: "id" >>= refineFail)
    <*> v .: "username"
    <*> v .: "slug"
    <*> ((v .: "avatar_url") >>= mkURIFail)
    <*> v .: "is_superuser"
    <*> v .: "is_staff"

data APIComment
  = APIComment
    { _commentId :: CommentId
    , _commentGalleryId :: GalleryId
    , _commentPoster :: APIPoster
    , _commentPostDate :: UTCTime
    , _commentBody :: T.Text
    }
  deriving (Show, Eq)

makeLenses ''APIComment

instance HasAPIPoster APIComment where
  apiPoster = commentPoster

instance FromJSON APIComment where
  parseJSON = withObject "APIComment" $ \v -> APIComment
    <$> (v .: "id" >>= refineFail)
    <*> (v .: "gallery_id" >>= refineFail)
    <*> v .: "poster"
    <*> (secondsToUTCTime <$> (v .: "post_date"))
    <*> v .: "body"
