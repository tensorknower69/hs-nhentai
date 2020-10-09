{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Data.Aeson
import Data.NHentai.API
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Refined
import Text.URI (render)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

main :: IO ()
main = do
	url <- refineThrow 41852 >>= mkGalleryApiUrl
	mgr <- newManager tlsManagerSettings
	req <- parseRequest (T.unpack $ render url)
	rep <- httpLbs req mgr 
	print rep
	let a = eitherDecode @APIGallery (responseBody rep)
	print a

