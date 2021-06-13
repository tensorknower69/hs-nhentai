{-# LANGUAGE LambdaCase #-}

module Data.NHentai.Internal.Utils where

import Control.Exception.Base
import Data.Char
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Text.URI
import qualified Data.Text as T

capitalize :: [Char] -> [Char]
capitalize [] = []
capitalize (a:as) = toUpper a : map toLower as

leftFail :: (Exception a, MonadFail f) => Either a b -> f b
leftFail = \case
  Left a -> fail $ show a
  Right b -> pure b

mkURIFail :: MonadFail m => T.Text -> m URI
mkURIFail = leftFail . mkURI

secondsToUTCTime :: Integer -> UTCTime
secondsToUTCTime = posixSecondsToUTCTime . secondsToNominalDiffTime . fromIntegral
