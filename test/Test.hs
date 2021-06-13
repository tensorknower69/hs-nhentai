{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Monad
import Data.Aeson
import Data.Maybe
import Data.NHentai.API.Comment
import Data.NHentai.API.Gallery
import Data.NHentai.Scraper.HomePage
import Data.NHentai.Types
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
      [ testGalleryApi $$(refineTH 177013) -- test for a normal gallery
      ]
    , testGroup "Comment"
      [ testCommentApi $$(refineTH 2) -- test for multilingual stuff
      ]
    ]
  , testGroup "Scraper"
    [ testGroup "HomePage"
      [ testHomePage False $$(refineTH 1) -- test first page
      , testHomePage False $$(refineTH 10000) -- test some random page
      , testHomePage True $$(refineTH 10000000) -- should be dead
      ]
    ]
  ]

httpJson :: FromJSON a => String -> IO (Either String a)
httpJson uri = do
  mgr <- newManager tlsManagerSettings
  req <- parseRequest uri
  rep <- httpLbs req mgr
  pure $ eitherDecode (responseBody rep)

testGalleryApi :: GalleryId -> TestTree
testGalleryApi gid = testCase ("api/gallery/" <> show (unrefine gid)) $ do
  uri <- mkGalleryApiUri gid
  httpJson @APIGalleryResult (renderStr uri) >>= \case
    Right (APIGalleryResultError _) -> assertFailure $ "Gallery is dead"
    Right (APIGalleryResultSuccess _) -> pure ()
    Left err -> assertFailure $ "Fail to parse json: " <> show err

testCommentApi :: GalleryId -> TestTree
testCommentApi gid = testCase ("api/gallery/" <> show (unrefine gid) <> "/comments") $ do
  uri <- mkCommentApiUri gid
  httpJson @[APIComment] (renderStr uri) >>= \case
    Right _ -> pure ()
    Left err -> assertFailure $ "Fail to parse json: " <> show err

testHomePage :: Bool -> PageIndex -> TestTree
testHomePage should_fail pid = testCase ("/?page=" <> show (unrefine pid)) $ do
  uri <- renderStr <$> mkHomePageUri pid
  result <- scrapeURL @String uri homePageScraper
  if should_fail then
    when (isJust result) $ do
      assertFailure $ "Expecting scrap home page failure, but succeeded: " <> show uri
  else
    when (result == Nothing) $ do
      assertFailure $ "Fail to scrap home page: " <> show uri

main :: IO ()
main = defaultMain tests
