{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import Control.Monad
import Data.Aeson
import Data.NHentai.API.Comment
import Data.NHentai.API.Gallery
import Data.NHentai.Scraper.HomePage
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Refined
import Test.Tasty
import Test.Tasty.HUnit
import Text.HTML.Scalpel
import Text.URI (renderStr)

tests :: TestTree
tests = testGroup "NHentai"
	[ testGroup "API"
		[ testGroup "Gallery"
			[ testGalleryApi 177013
			, testGalleryApi 74159
			, testGalleryApi 1
			, testGalleryApi 326102
			]
		, testGroup "Comment"
			[ testCommentApi 2
			, testCommentApi 16162
			]
		]
	, testGroup "Scraper"
		[ testGroup "HomePage"
			[ testHomePage 1
			, testHomePage 10000
			, testHomePage 10000000
			]
		]
	]

httpJson :: FromJSON a => String -> IO (Either String a)
httpJson url = do
	mgr <- newManager tlsManagerSettings
	req <- parseRequest url
	rep <- httpLbs req mgr
	pure $ eitherDecode (responseBody rep)

testGalleryApi :: Int -> TestTree
testGalleryApi gid = testCase ("api/gallery/" <> show gid) $ do
	refineFail gid >>= mkGalleryApiUrl >>= httpJson @APIGallery . renderStr >>= \case
		Right _ -> pure ()
		Left err -> assertFailure $ "fail to parse json: " <> show err

testCommentApi :: Int -> TestTree
testCommentApi gid = testCase ("api/gallery/" <> show gid <> "/comments") $ do
	refineFail gid >>= mkCommentApiUrl >>= httpJson @[APIComment] . renderStr >>= \case
		Right _ -> pure ()
		Left err -> assertFailure $ "fail to parse json: " <> show err

testHomePage :: Int -> TestTree
testHomePage page = testCase ("/?page=" <> show page) $ do
	url <- renderStr <$> (refineFail page >>= mkHomePageUrl)
	result <- scrapeURL @String url homePageScraper
	when (result == Nothing) $ assertFailure $ "fail to scrap home page: " <> show url

main :: IO ()
main = defaultMain tests
