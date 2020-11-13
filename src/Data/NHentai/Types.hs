{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.NHentai.Types where

import Control.Lens
import Data.Char
import Refined
import qualified Data.Text as T

-- |The ID of a poster/user.
type PosterID = Refined Positive Int
-- |The ID of a comment.
type CommentID = Refined Positive Int
-- |@https://nhentai.net/g/<gallery_id>@
type GalleryID = Refined Positive Int
-- |The ID of a tag.
type TagID = Refined Positive Int
-- |The media id of a gallery, see https://nhentai.net/api/gallery/177013, "media_id" field.
-- I am not entirely sure why nhentai have this identifier.
type MediaID = Refined Positive Int
-- |Page number, starts from 1, like @https://nhentai.net/g/177013/<page_id>@
type PageIndex = Refined Positive Int
-- |The tag types, this is what you see next to the cover page of a gallery when viewing one.
data TagType
	= TagTag
	| LanguageTag
	| CategoryTag
	| CharacterTag
	| GroupTag
	| ArtistTag
	| ParodyTag
	deriving (Show, Eq, Read)

data ImageType
	= JPG -- ^.jpeg type
	| PNG -- ^.png type
	| GIF -- ^.gif type, pretty rare.
	deriving (Show, Eq, Read)

extension :: Prism' String ImageType
extension = prism' a b
	where
	a = map toLower . show
	b (c : _) = c ^? extensionChar
	b _ = Nothing

extensionChar :: Prism' Char ImageType
extensionChar = prism' a b
	where
	a = toLower . head . show
	b 'j' = Just JPG
	b 'p' = Just PNG
	b 'g' = Just GIF
	b _ = Nothing


data ImageSpec
	-- ^An ImageSpec is basically an image's metadata. Mostly used for finding out the image type of an image.
	= ImageSpec
		{ _eitherImageType :: Either String ImageType -- ^The image type of the image spec, sometimes the image type is '0' or '1' or '2' or other stuff. That's why this is an Either.
		, _width :: Refined Positive Int -- ^The width of the image.
		, _height :: Refined Positive Int -- ^The height of the image.
		}
	deriving (Show, Eq)

makeLenses ''ImageSpec

class HasGalleryID a where
	galleryId :: Lens' a GalleryID

class HasMediaID a where
	mediaId :: Lens' a MediaID

class HasTitle a where
	title :: Lens' a T.Text
