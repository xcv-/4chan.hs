module FourChan.Attachment where

import Data.Aeson
import Data.Aeson.Types
import Data.Text (pack)

import Text.Printf (printf)

import FourChan.Formatable
import FourChan.Helpers.StringPiece


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
        renamedFileName <- fmap show (m .: pack "tim" :: Parser Int)
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
    fchar 'r' _ = Just . SameLine . getRenamedFileName
    fchar 'n' _ = Just . SameLine . getFileName
    fchar 'e' _ = Just . SameLine . getFileExtension
    fchar '5' _ = Just . SameLine . getFileMD5
    fchar 'w' _ = Just . SameLine . show . getImageWidth
    fchar 'h' _ = Just . SameLine . show . getImageHeight
    fchar 'd' _ = fmap SameLine . fmtFileDeleted
        where fmtFileDeleted att = if isFileDeleted att
                                   then Just "[deleted]"
                                   else Nothing
    fchar c arg = error $
        printf "Unknown format specifier '%c' with argument \"%s\"" c arg

