module FourChan.Helpers.Json where

import Data.JSON2
import Data.JSON2.Parser
import qualified Data.Map as M

jsonLookup :: (FromJson a) => M.Map String Json -> String -> String -> a
jsonLookup m typeName key =
    case M.lookup key m of
        Just x -> fromJson x
        Nothing -> error
            $ "Could not find '" ++ key
            ++ "' in " ++  typeName ++ " json"
            ++ "\nMap: " ++ show m

safeParseJson :: String -> Json
safeParseJson = ensureRight . parseJson
    where
        ensureRight (Right x) = x
        ensureRight (Left x) = error $ "Parse error: " ++ show x
