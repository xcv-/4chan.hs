module FourChan.Filters
( Filter(..)
, Filters(..)
, findThread
, formatWithLinks
) where

import Control.Monad

import qualified Data.Sequence as Seq

import Text.Regex.PCRE
import Text.Regex.PCRE.Sequence

import FourChan.Thread

import FourChan.Format.Formatable
import FourChan.Format.StringPiece

data Filter a = Filter { getter  :: a -> String
                       , pattern :: String
                       }

type Patter = String

data Filters a = All [Filter a] | Any [Filter a]

checkFilter :: a -> Filter a -> IO Bool
checkFilter x item = do
    Right compiled <- compile compCaseless execBlank pattern'
    return $ match compiled (getter item x)
  where
    pattern' = Seq.fromList $ pattern item

check :: Filters a -> a -> IO Bool
check (All fs) x = fmap and $ mapM (checkFilter x) fs
check (Any fs) x = fmap or  $ mapM (checkFilter x) fs


findThread :: String -> Filters Thread -> IO [Thread]
findThread board exprs = getThreadIndex board >>= filterM (check exprs)

formatWithLinks :: String -> [Thread] -> [String]
formatWithLinks board = concat . map (piecesLines . fmt)
    where
        fmt = format $ "%{" ++ board ++ "}L :: (%# replies) %{%t}o %{%B }?%{%l }?%{%#%{ %n}?%{ %$}?%{ -- %s}?}o"
