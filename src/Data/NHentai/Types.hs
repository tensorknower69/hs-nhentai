module Data.NHentai.Types where

import Data.Char
import Refined
import qualified Data.Text as T

type PosterID = Refined Positive Int
type CommentID = Refined Positive Int
type GalleryID = Refined Positive Int
type TagID = Refined Positive Int
type MediaID = Refined Positive Int
type PageIndex = Refined Positive Int

data TagType = TagTag | LanguageTag | CategoryTag | CharacterTag | GroupTag | ArtistTag | ParodyTag deriving (Show, Eq, Read)

data ImageSpec
	= ImageSpec
		{ type'ImageSpec :: ImageType
		, width'ImageSpec :: Refined Positive Int
		, height'ImageSpec :: Refined Positive Int
		}
	deriving (Show, Eq)

data ImageType = JPG | PNG | GIF deriving (Show, Eq, Read)

imageTypeExtension :: ImageType -> String
imageTypeExtension = map toLower . show
