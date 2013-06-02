module FourChan.Attachment where

import Data.Typeable
import Data.JSON2
import qualified Data.Map as M

import FourChan.Helpers.Json


data Attachment = Attachment
    { getRenamedFileName :: String
    , getFileName        :: String
    , getFileExtension   :: String
    , getFileMD5         :: String
    , getFileSize        :: Int
    , getImageWidth      :: Int
    , getImageHeight     :: Int
    , isFileDeleted      :: Bool
    } deriving (Eq, Show)

instance Typeable Attachment where
    typeOf _ = mkTyConApp (mkTyCon3 "4chan" "FourChan" "Attachment") []

instance FromJson Attachment where
    safeFromJson obj@(JObject m) = Right Attachment
        { getRenamedFileName = show $ lkpI' "tim"
        , getFileName        = lkpS "filename"
        , getFileExtension   = lkpS "ext"
        , getFileSize        = lkpI "fsize"
        , getFileMD5         = lkpS "md5"
        , getImageWidth      = lkpI "w"
        , getImageHeight     = lkpI "h"
        , isFileDeleted      = case fmap (((==1) :: Int -> Bool) . fromJson)
                                         (M.lookup "filedeleted" m) of
                                   Just x -> x
                                   _ -> False
        }
        where
            lkpI = jsonLookup m "Attachment" :: String -> Int
            lkpI' = jsonLookup m "Attachment" :: String -> Integer
            lkpS = jsonLookup m "Attachment" :: String -> String

    safeFromJson x = mkError x
