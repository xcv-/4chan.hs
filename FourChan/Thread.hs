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

import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as BSC
import Data.Maybe
import qualified Data.Map as M
import Data.Ratio
import Data.Text (pack)

import Text.Printf (printf)

import FourChan.Post
import FourChan.Board
import FourChan.Attachment
import FourChan.Helpers.Download


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

instance FromJSON Thread where
    parseJSON (Object m) = do
        (Object opM:ress) <- m .: pack "posts"
        (op:replies)   <- m .: pack "posts"

        numReplies    <- opM .:  pack "replies"  .!= 0
        numImages     <- opM .:  pack "images"   .!= 0
        omittedPosts  <- opM .:? pack "omitted_posts"  .!= 0
        omittedImages <- opM .:? pack "omitted_images" .!= 0

        let isOne = (==1) :: Int -> Bool
        sticky        <- fmap isOne $ opM .:? pack "sticky"     .!= 0
        closed        <- fmap isOne $ opM .:? pack "closed"     .!= 0
        bumpLimit     <- fmap isOne $ opM .:? pack "bumplimit"  .!= 0
        imageLimit    <- fmap isOne $ opM .:? pack "imagelimit" .!= 0
        return $ Thread
            { getOp            = op
            , getReplies       = replies
            , getNumReplies    = numReplies
            , getNumImages     = numImages
            , getOmittedPosts  = omittedPosts
            , getOmittedImages = omittedImages
            , isSticky         = sticky
            , isClosed         = closed
            , gotBumpLimit     = bumpLimit
            , gotImageLimit    = imageLimit
            }


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
        getBoardPage pageNum =
            let page = download $ boardPageUrl boardName pageNum
            in  fmap (either err getPageThreads . eitherDecode') page
        err msg = error $ "Error decoding board page: " ++ msg


getThread :: String -> Int -> IO Thread
getThread boardName threadId =
    let thread = download $ threadUrl boardName threadId
    in  fmap (either err id . eitherDecode') thread
    where
        err msg = error $ "Error decoding thread: " ++ msg


getPageThreads :: M.Map String [Thread] -> [Thread]
getPageThreads = maybe err id . M.lookup "threads"
    where
        err = error $ "Could not parse board page"
