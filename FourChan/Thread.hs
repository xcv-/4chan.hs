module FourChan.Thread
( Thread
, getOp
, getReplies

, getNumReplies
, getNumImages
, getOmittedPosts
, getOmittedImages

, isSticky
, isClosed
, gotBumpLimit
, gotImageLimit

, getPosts
, getAttachments

, getThreadIndex
, getThread
) where

import qualified Control.Monad.Parallel as Parallel

import Data.Maybe
import Data.Ratio
import Data.Typeable
import Data.JSON2
import qualified Data.Map as M

import Text.Printf (printf)

import FourChan.Post
import FourChan.Board
import FourChan.Attachment
import FourChan.Helpers.Download
import FourChan.Helpers.Json


threadUrl :: String -> Int -> String
threadUrl = printf "http://api.4chan.org/%s/res/%d.json"

boardPageUrl :: String -> Int -> String
boardPageUrl = printf "http://api.4chan.org/%s/%d.json"


data Thread = Thread
    { getOp            :: Post
    , getReplies       :: [Post]
    , getNumReplies    :: Int
    , getNumImages     :: Int
    , getOmittedPosts  :: Int
    , getOmittedImages :: Int
    , isSticky         :: Bool
    , isClosed         :: Bool
    , gotBumpLimit     :: Bool
    , gotImageLimit    :: Bool
    } deriving (Eq, Show)

instance Typeable Thread where
    typeOf _ = mkTyConApp (mkTyCon3 "4chan" "FourChan" "Thread") []

instance FromJson Thread where
    safeFromJson (JObject m) =
        case M.lookup "posts" m of
            Just (JArray (op:replies)) ->

                let JObject opMap = op
                    lkpI          = jsonLookup opMap "OP" :: String -> Int
                    lkpDefaultI k = fromJson
                        (M.findWithDefault (JNumber (0%1)) k opMap) :: Int

                in Right Thread
                    { getOp            = fromJson op
                    , getReplies       = map fromJson replies
                    , getNumReplies    = lkpI "replies"
                    , getNumImages     = lkpI "images"
                    , getOmittedPosts  = lkpDefaultI "omitted_posts"
                    , getOmittedImages = lkpDefaultI "omitted_images"
                    , isSticky         = lkpDefaultI "sticky" == 1
                    , isClosed         = lkpDefaultI "closed" == 1
                    , gotBumpLimit     = lkpI "bumplimit"  == 1
                    , gotImageLimit    = lkpI "imagelimit" == 1
                    }
            x -> mkError x

    safeFromJson x = mkError x


getPosts :: Thread -> [Post]
getPosts thread = getOp thread : getReplies thread

getAttachments :: Thread -> [Attachment]
getAttachments = catMaybes . map getAttachment . getPosts



getThreadIndex :: String -> IO [Thread]
getThreadIndex boardName = do
    board <- getBoardInfo boardName
    fmap concat $ Parallel.mapM getBoardPage [0..getNumPages board -1]
    where
        getBoardPage :: Int -> IO [Thread]
        getBoardPage = fmap (getPageThreads . safeParseJson) . download . url
        url :: Int -> String
        url = boardPageUrl boardName

getThread :: String -> Int -> IO Thread
getThread boardName threadId = fmap (fromJson . safeParseJson) $ download url
    where
        url :: String
        url = threadUrl boardName threadId


getPageThreads :: Json -> [Thread]
getPageThreads (JObject m) =
    case M.lookup "threads" m of
        Just (JArray a) -> map (fromJson :: Json -> Thread) a
        x -> error $ "Unknown " ++ show x ++ " in " ++ show m
getPageThreads x = error $ "getPageThreads: Invalid parameter: " ++ show x
