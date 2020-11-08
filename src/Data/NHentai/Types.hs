module Data.NHentai.Types where

import Data.Char
import Refined

type PosterID = Refined Positive Int
type CommentID = Refined Positive Int
type GalleryID = Refined Positive Int
type TagID = Refined Positive Int
type MediaID = Refined Positive Int
type PageIndex = Refined Positive Int

data TagType = TagTag | LanguageTag | CategoryTag | CharacterTag | GroupTag | ArtistTag | ParodyTag deriving (Show, Eq, Read)

data ImageSpec
	= ImageSpec
		{ eitherImageType'ImageSpec :: Either String ImageType
		, width'ImageSpec :: Refined Positive Int
		, height'ImageSpec :: Refined Positive Int
		}
	deriving (Show, Eq)

data ImageType = JPG | PNG | GIF deriving (Show, Eq, Read)

imageTypeToExtension :: ImageType -> String
imageTypeToExtension = map toLower . show

extensionToImageType :: String -> Maybe ImageType
extensionToImageType (a : _) = charToImageType a
extensionToImageType _ = Nothing

charToImageType :: Char -> Maybe ImageType
charToImageType 'j' = Just JPG
charToImageType 'p' = Just PNG
charToImageType 'g' = Just GIF
charToImageType _ = Nothing
