module FourChan.Helpers.Download
( download
, downloadBytes
) where

import Data.ByteString (ByteString)

import Network.HTTP
import Network.URI (URI, parseURI)

download :: String -> IO String
download url = simpleHTTP (getRequest url) >>= getResponseBody

downloadBytes :: String -> IO ByteString
downloadBytes url = downloadBytes' $ parseURI url
    where
        downloadBytes' :: Maybe URI -> IO ByteString
        downloadBytes' (Just uri) =
            simpleHTTP (defaultGETRequest_ uri) >>= getResponseBody
        downloadBytes' Nothing = error $ "Invalid URI: " ++ url
