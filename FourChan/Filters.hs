module FourChan.Filters
( FilterItem(..)
, Filter(..)
, findThread
, formatWithLinks
) where

import Control.Monad

import qualified Data.Sequence as Seq

import Text.Regex.PCRE
import Text.Regex.PCRE.Sequence

import FourChan.Formatable
import FourChan.Thread

import FourChan.Helpers.StringPiece

type FilterItem a = (a -> String, String)

data Filter a = All [FilterItem a] | Any [FilterItem a]

checkItem :: a -> FilterItem a -> IO Bool
checkItem x (getter, rx) = do
    Right compiled <- compile compCaseless execBlank (Seq.fromList rx)
    return $ match compiled (getter x)

check :: Filter a -> a -> IO Bool
check (All fs) x = fmap and $ mapM (checkItem x) fs
check (Any fs) x = fmap or $ mapM (checkItem x) fs


findThread :: String -> Filter Thread -> IO [Thread]
findThread board exprs = getThreadIndex board >>= filterM (check exprs)

formatWithLinks :: String -> [Thread] -> [String]
formatWithLinks board = concat . map (piecesLines . fmt)
    where
        fmt = format $ "%{" ++ board ++ "}L :: (%# replies) %{%t}o %{%B }?%{%l }?%{%#%{ %n}?%{ %$}?%{ -- %s}?}o"
