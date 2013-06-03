module FourChan.Board
( Board
, getBoardName
, getTitle
, getNumPages
, getNumThreadsPerPage

, getBoardsInfo
, getBoardInfo
) where

import Data.Aeson
import Data.Foldable (find)
import Data.Maybe
import qualified Data.Map as M
import Data.Text (pack)

import FourChan.Helpers.Download


boardsUrl :: String
boardsUrl = "http://api.4chan.org/boards.json"


data Board = Board
    { getBoardName         :: String
    , getTitle             :: String
    , getNumPages          :: Int
    , getNumThreadsPerPage :: Int
    } deriving (Eq, Show)

instance FromJSON Board where
    parseJSON (Object m) = do
        boardName         <- m .: pack "board"
        title             <- m .: pack "title"
        numPages          <- m .: pack "pages"
        numThreadsPerPage <- m .: pack "per_page"
        return $ Board
            { getBoardName         = boardName
            , getTitle             = title
            , getNumPages          = numPages
            , getNumThreadsPerPage = numThreadsPerPage
            }


getBoardsInfo :: IO [Board]
getBoardsInfo = fmap (maybe err (getBoards . fromJust) . decode) $ download boardsUrl
    where
        err = error "Error decoding board index JSON"

getBoardInfo :: String -> IO Board
getBoardInfo name = fmap findBoard getBoardsInfo
    where
        findBoard :: [Board] -> Board
        findBoard = maybe err id . find (\b -> getBoardName b == name)
        err =  error $ "Board " ++ name ++ " does not exist"


getBoards :: M.Map String [Board] -> [Board]
getBoards = maybe err id . M.lookup "boards"
    where
        err = error $ "Could not parse board list"
