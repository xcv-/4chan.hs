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

import Data.Aeson
import Data.Aeson.Types
import Data.Maybe
import Data.Ratio
import Data.Text (pack)

import Text.Printf

import FourChan.Attachment
import FourChan.Formatable
import FourChan.Helpers.Html
import FourChan.Helpers.Download
import FourChan.Helpers.StringPiece


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
        threadId      <- m .: pack "resto"
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
    fchar '#' _ = Just . SameLine . show . getPostId
    fchar 'T' _ = Just . SameLine . show . getTimestamp
    fchar 't' _ = Just . SameLine . getTime
    fchar 'n' _ = fmap SameLine . getPosterName
    fchar '$' _ = fmap SameLine . getTrip
    fchar 'e' _ = fmap SameLine . getEmail
    fchar 's' _ = fmap SameLine . getSubject
    fchar 'h' _ = fmap SameLine . getHtmlComment
    fchar 'c' _ = fmap (MultiLine . lines) . getPlainTextComment
    fchar 'a' fmt = fmap (NestedPieces . format fmt) . getAttachment
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
