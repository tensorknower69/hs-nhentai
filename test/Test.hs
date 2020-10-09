{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Data.Aeson
import Data.Either
import Data.NHentai.API
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Refined
import Test.Tasty
import Test.Tasty.HUnit
import Text.URI (render)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

tests :: TestTree
tests = testGroup "testing Data.NHentai.API" [testGalleryApi 41753]

testGalleryApi :: Int -> TestTree
testGalleryApi gid = testCase ("download " <> show gid) $ do
	url <- refineThrow gid >>= mkGalleryApiUrl
	mgr <- newManager tlsManagerSettings
	req <- parseRequest (T.unpack $ render url)
	rep <- httpLbs req mgr 
	let result = eitherDecode @APIGallery (responseBody rep)
	assertBool "can decode json" (isRight result)

main :: IO ()
main = defaultMain tests
