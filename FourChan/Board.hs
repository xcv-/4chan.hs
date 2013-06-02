module FourChan.Board
( Board
, getBoardName
, getTitle
, getNumPages
, getNumThreadsPerPage

, getBoardsInfo
, getBoardInfo
) where

import Data.JSON2
import Data.Foldable (find)
import qualified Data.Map as M
import Data.Typeable

import FourChan.Helpers.Json
import FourChan.Helpers.Download


boardsUrl :: String
boardsUrl = "http://api.4chan.org/boards.json"


data Board = Board
    { getBoardName         :: String
    , getTitle             :: String
    , getNumPages          :: Int
    , getNumThreadsPerPage :: Int
    } deriving (Eq, Show)

instance Typeable Board where
    typeOf _ = mkTyConApp (mkTyCon3 "4chan" "FourChan" "Board") []

instance FromJson Board where
    safeFromJson (JObject m) = Right Board
        { getBoardName         = lkpS "board"
        , getTitle             = lkpS "title"
        , getNumPages          = lkpI "pages"
        , getNumThreadsPerPage = lkpI "per_page"
        }
        where
            lkpI = jsonLookup m "Board"
            lkpS = jsonLookup m "Board"

    safeFromJson x = mkError x


getBoardsInfo :: IO [Board]
getBoardsInfo = fmap (getBoards . safeParseJson) (download boardsUrl)

getBoardInfo :: String -> IO Board
getBoardInfo name = fmap findBoard getBoardsInfo
    where
        findBoard :: [Board] -> Board
        findBoard = ensureJust . find (\b -> getBoardName b == name)
        ensureJust :: Maybe a -> a
        ensureJust (Just result) = result
        ensureJust Nothing = error $ "Board " ++ name ++ " does not exist"


getBoards :: Json -> [Board]
getBoards (JObject m) =
    case M.lookup "boards" m of
        Just (JArray a) -> map (fromJson :: Json -> Board) a
        x -> error $ "Unknown " ++ show x ++ " in " ++ show m
getBoards x = error $ "getBoards: Invalid parameter: " ++ show x
