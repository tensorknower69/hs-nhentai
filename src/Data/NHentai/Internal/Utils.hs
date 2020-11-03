{-# LANGUAGE LambdaCase #-}

module Data.NHentai.Internal.Utils where

import Control.Exception.Base
import Data.Char

capitalize :: [Char] -> [Char]
capitalize [] = []
capitalize (a:as) = toUpper a : map toLower as

leftFail :: (Exception a, MonadFail f) => Either a b -> f b
leftFail = \case
	Left a -> fail $ show a
	Right b -> pure b
