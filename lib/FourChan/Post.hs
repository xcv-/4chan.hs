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

import Control.Applicative

import Data.Aeson
import Data.Aeson.Types
import Data.Maybe
import Data.Ratio
import Data.Text (pack)

import Text.Printf (printf)

import FourChan.Attachment
import FourChan.Format.Formatable
import FourChan.Format.StringPiece
import FourChan.Helpers.Html
import FourChan.Helpers.Download


postLink :: String -> Int -> Int -> String
postLink = printf "https://boards.4chan.org/%s/res/%d#p%d"


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

instance FromJSON Post where
    parseJSON o@(Object m) = do
        let isZero = (==0) :: Int -> Bool
        op            <- fmap isZero $ m .: pack "resto"
        postId        <- m .: pack "no"
        threadId'     <- m .: pack "resto"
        timestamp     <- m .: pack "time"
        time          <- m .: pack "now"
        posterName    <- m .:? pack "name"
        trip          <- m .:? pack "trip"
        email         <- m .:? pack "email"
        subject       <- m .:? pack "sub"
        htmlComment   <- m .:? pack "com"
        filename      <- m .:? pack "filename" :: Parser (Maybe String)
        attachment    <- case filename of
                             Just _  -> fmap Just $ parseJSON o
                             Nothing -> return Nothing

        let threadId = if isZero threadId'
                           then postId
                           else threadId'

        return $ Post
            { getPostId      = postId
            , getThreadId    = threadId
            , getTimestamp   = timestamp
            , isOp           = op
            , getTime        = time
            , getPosterName  = posterName
            , getTrip        = trip
            , getEmail       = email
            , getSubject     = subject
            , getHtmlComment = htmlComment
            , getAttachment  = attachment
            }

instance Formatable Post where
    fchar '#' _ = return . SameLine . show . getPostId
    fchar 'T' _ = return . SameLine . show . getTimestamp
    fchar 't' _ = return . SameLine . getTime
    fchar 'n' _ = maybe (fail "No name")    (return . SameLine) . getPosterName
    fchar '$' _ = maybe (fail "No trip")    (return . SameLine) . getTrip
    fchar 'e' _ = maybe (fail "No email")   (return . SameLine) . getEmail
    fchar 's' _ = maybe (fail "No subject") (return . SameLine) . getSubject
    fchar 'h' _ = maybe (fail "No comment") (return . SameLine) . getHtmlComment

    fchar 'c' _ = maybe (fail "No comment") (return . MultiLine . lines) .
        getPlainTextComment

    fchar 'a' fmt = maybe (fail "No attachment") (return .NestedPieces . format fmt) .
        getAttachment

    fchar 'L' board = return . SameLine .
            (postLink board <$> getThreadId <*> getPostId)

    fchar '^' _ = fmap (SameLine . show) .
            (\post -> if isOp post
                         then fail "Post is OP"
                         else return (getThreadId post))

    fchar 'o' _ = fmap SameLine .
            (\post -> if isOp post
                         then return "[OP]"
                         else fail "Post is not OP")

    fchar c arg = fcharError c arg


hasAttachment :: Post -> Bool
hasAttachment Post { getAttachment = Just _ } = True
hasAttachment _ = False

getPlainTextComment :: Post -> Maybe String
getPlainTextComment = fmap plainText . getHtmlComment
