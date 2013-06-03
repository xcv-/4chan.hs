module FourChan.Helpers.Download
( download
) where

import Data.Char
import qualified Data.ByteString.Lazy as BS

import Network.HTTP
import Network.URI (URI, parseURI)

download :: String -> IO BS.ByteString
download url = do
    downloadBytes $ parseURI url
    where
        downloadBytes :: Maybe URI -> IO BS.ByteString
        downloadBytes (Just uri) =
            simpleHTTP (defaultGETRequest_ uri) >>= getResponseBody
        downloadBytes Nothing = error $ "Invalid URI: " ++ url
