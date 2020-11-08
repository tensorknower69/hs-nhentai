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
		{ type'ImageSpec :: Maybe ImageType
		, width'ImageSpec :: Refined Positive Int
		, height'ImageSpec :: Refined Positive Int
		}
	deriving (Show, Eq)

data ImageType = JPG | PNG | GIF deriving (Show, Eq, Read)

imageTypeToExtension :: ImageType -> String
imageTypeToExtension = map toLower . show

extensionToImageType :: String -> Maybe (Maybe ImageType)
extensionToImageType (a : _) = charToImageType a
extensionToImageType _ = Nothing

-- | Nothing: unknown image type
-- Just Nothing: the image is invalid, and therefore unknown image type
-- Just (Just x): x is an actual image type
charToImageType :: Char -> Maybe (Maybe ImageType)
charToImageType 'j' = Just $ Just JPG
charToImageType 'p' = Just $ Just PNG
charToImageType 'g' = Just $ Just GIF
charToImageType '0' = Just Nothing -- e.g. https://i.nhentai.net/galleries/900513/2.00
charToImageType _ = Nothing
