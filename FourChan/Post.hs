module FourChan.Post
( Post
, getPostId
, getThreadId
, getTimestamp
, isOp
, getTime
, getPosterName
, getTrip
, getEmail
, getSubject
, getHtmlComment
, getAttachment

, hasAttachment
, getPlainTextComment
) where

import Debug.Trace (trace)

import Data.JSON2
import Data.Maybe
import Data.Ratio
import Data.Typeable
import qualified Data.Map as M

import Text.Printf

import FourChan.Attachment
import FourChan.Formatable
import FourChan.Helpers.Json
import FourChan.Helpers.Html
import FourChan.Helpers.Download
import FourChan.Helpers.String


data Post = Post
    { getPostId      :: Int
    , getThreadId    :: Int
    , getTimestamp   :: Int
    , isOp           :: Bool
    , getTime        :: String
    , getPosterName  :: Maybe String
    , getTrip        :: Maybe String
    , getEmail       :: Maybe String
    , getSubject     :: Maybe String
    , getHtmlComment :: Maybe String
    , getAttachment  :: Maybe Attachment
    } deriving (Eq, Show)

instance Typeable Post where
    typeOf _ = mkTyConApp (mkTyCon3 "4chan" "FourChan" "Post") []

instance FromJson Post where
    safeFromJson obj@(JObject m) = Right Post
        { getPostId      = lkpI "no"
        , getThreadId    = lkpI "resto"
        , getTimestamp   = lkpI "time"
        , isOp           = lkpI "resto" == 0
        , getTime        = lkpS "now"
        , getPosterName  = fmap fromJson $ M.lookup "name"  m
        , getTrip        = fmap fromJson $ M.lookup "trip"  m
        , getEmail       = fmap fromJson $ M.lookup "email" m
        , getSubject     = fmap fromJson $ M.lookup "sub"   m
        , getHtmlComment = fmap fromJson $ M.lookup "com"   m
        , getAttachment  = M.lookup "filename" m >> return (fromJson obj)
        }
        where
            lkpI = jsonLookup m "Post" :: String -> Int
            lkpS = jsonLookup m "Post" :: String -> String

    safeFromJson x = mkError x

instance Formatable Post where
    fchar '#' _ = Just . SameLine . show . getPostId
    fchar 'T' _ = Just . SameLine . show . getTimestamp
    fchar 't' _ = Just . SameLine . getTime
    fchar 'n' _ = fmap SameLine . getPosterName
    fchar '$' _ = fmap SameLine . getTrip
    fchar 'e' _ = fmap SameLine . getEmail
    fchar 's' _ = fmap SameLine . getSubject
    fchar 'h' _ = fmap SameLine . getHtmlComment
    fchar 'c' _ = fmap (MultiLine . lines) . getPlainTextComment
    {-fchar 'a' fmt = fmap (NestedPieces . format fmt) . getAttachment-}
    fchar '^' _ = fmap (SameLine . show) . filterOp
        where
            filterOp post = if isOp post then Nothing else Just (getThreadId post)
    fchar 'o' _ = fmap SameLine . opfmt
        where
            opfmt post = if isOp post then Just "[OP]" else Nothing
    fchar c arg = error $
        printf "Unknown format specifier '%c' with argument \"%s\"" c arg


hasAttachment :: Post -> Bool
hasAttachment Post { getAttachment = Just _ } = True
hasAttachment _ = False

getPlainTextComment :: Post -> Maybe String
getPlainTextComment = fmap plainText . getHtmlComment
