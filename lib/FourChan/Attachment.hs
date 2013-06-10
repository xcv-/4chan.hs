module FourChan.Attachment where

import Control.Applicative

import Data.Aeson
import Data.Aeson.Types
import Data.Text (pack)

import Text.Printf (printf)

import FourChan.Format.Formatable
import FourChan.Format.StringPiece


attachmentLink :: String -> String -> String -> String
attachmentLink = printf "https://images.4chan.org/%s/src/%s%s"


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

instance FromJSON Attachment where
    parseJSON (Object m) = do
        renamedFileName <- fmap show (m .: pack "tim" :: Parser Integer)
        fileName        <- m .: pack "filename"
        fileExtension   <- m .: pack "ext"
        fileSize        <- m .: pack "fsize"
        fileMD5         <- m .: pack "md5"
        imageWidth      <- m .: pack "w"
        imageHeight     <- m .: pack "h"
        fileDeleted     <- m .:? pack "filedeleted" :: Parser (Maybe Int)
        return $ Attachment
            { getRenamedFileName = renamedFileName
            , getFileName        = fileName
            , getFileExtension   = fileExtension
            , getFileSize        = fileSize
            , getFileMD5         = fileMD5
            , getImageWidth      = imageWidth
            , getImageHeight     = imageHeight
            , isFileDeleted      = maybe False (==1) fileDeleted
            }

instance Formatable Attachment where
    fchar 'r' _ = return . SameLine . getRenamedFileName
    fchar 'n' _ = return . SameLine . getFileName
    fchar 'e' _ = return . SameLine . getFileExtension
    fchar '5' _ = return . SameLine . getFileMD5
    fchar 'w' _ = return . SameLine . show . getImageWidth
    fchar 'h' _ = return . SameLine . show . getImageHeight
    fchar 'd' _ = fmap SameLine . fmtFileDeleted
        where fmtFileDeleted att = if isFileDeleted att
                                   then return "[deleted]"
                                   else fail "File does not exist"
    fchar 'L' board = return . SameLine . makeLink
        where
            makeLink = attachmentLink board <$> getRenamedFileName <*> getFileExtension
    fchar c arg = fcharError c arg

