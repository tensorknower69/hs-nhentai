{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Data.Aeson
import Data.NHentai.API.Gallery
import Data.NHentai.API.Comment
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Refined
import Test.Tasty
import Test.Tasty.HUnit
import Text.URI (renderStr)

tests :: TestTree
tests = testGroup "Data.NHentai" [apiTests]

apiTests :: TestTree
apiTests = testGroup "testing Data.NHentai.API"
	[ galleryApiTests
	, commentApiTests
	]

galleryApiTests :: TestTree
galleryApiTests = testGroup "testing Data.NHentai.API.Gallery"
	[ testGalleryApi 177013
	, testGalleryApi 74159
	, testGalleryApi 1
	, testGalleryApi 326102
	]

commentApiTests :: TestTree
commentApiTests = testGroup "testing Data.NHentai.API.Comment"
	[ testCommentApi 177013
	, testCommentApi 1
	, testCommentApi 16162
	]

httpJson :: FromJSON a => String -> IO (Either String a)
httpJson url = do
	mgr <- newManager tlsManagerSettings
	req <- parseRequest url
	rep <- httpLbs req mgr
	pure $ eitherDecode (responseBody rep)

testGalleryApi :: Int -> TestTree
testGalleryApi gid = testCase ("decode api/gallery/" <> show gid) $ do
	result <- refineFail gid >>= mkGalleryApiUrl >>= httpJson @APIGallery . renderStr
	case result of
		Right _ -> pure ()
		Left err -> do
			assertFailure $ "fail to parse json: " <> show err

testCommentApi :: Int -> TestTree
testCommentApi gid = testCase ("decode api/gallery/" <> show gid <> "/comments") $ do
	result <- refineFail gid >>= mkCommentApiUrl >>= httpJson @[APIComment] . renderStr
	case result of
		Right _ -> pure ()
		Left err -> do
			assertFailure $ "fail to parse json: " <> show err

main :: IO ()
main = defaultMain tests
