module FourChan.Formatable
( Formatable
, fchar
, fcharError
, format
) where

import Control.Applicative
import Control.Monad

import Data.Char
import Data.Either
import Data.List

import Text.Printf

import FourChan.Helpers.StringPiece


class Formatable a where
    {-fhelp :: a -> [(Char,String)]-}
    fchar :: Char -> String -> a -> Maybe StringPiece
    fcharError :: Char -> String -> a -> Maybe StringPiece
    fcharError c arg = error $
        printf "Unknown format specifier '%c' with argument \"%s\"" c arg


fmapEither :: (a -> b) -> Either a a -> Either b b
fmapEither f (Left x)  = Left (f x)
fmapEither f (Right x) = Right (f x)


format :: Formatable a => String -> a -> [StringPiece]
format fmt f = either id id $ format' fmt f

format' :: Formatable a => String -> a -> Either [StringPiece] [StringPiece]
format' [] f = Right []
format' ('%':[]) f = error "Invalid character '%' at the end of the format string"
format' ('%':s) f = let (item, s') = formatItem s f
                    in case item of
                           Nothing -> Left $ format s' f
                           Just it -> fmapEither (it:) $ format' s' f

format' s f = let (plain, cont) = break (=='%') s
              in fmapEither (SameLine plain :) $ format' cont f


formatItem :: Formatable f => String -> f -> (Maybe StringPiece, String)
formatItem ('%':s) _ = (Just $ SameLine "%", s)
formatItem ('!':s) _ = (Just Break, s)

formatItem ('(':s) f = let (sep, s') = matchBrace '(' s
                           fmtSep    = NestedPieces $ format sep f
                           (x, s'')  = formatItem s' f
                       in (x >>= formatNested fmtSep, s'')
    where
        formatNested :: StringPiece -> StringPiece -> Maybe StringPiece
        formatNested sep (NestedPieces ps) = let ps' = intersperse sep ps
                                             in Just $ NestedPieces ps'
        formatNested _ _ = Nothing

formatItem ('[':s) f = let (fill, s')     = matchBrace '[' s
                           (pre, post)    = splitList "%@" fill
                           (pre', post')  = (Column pre, Column post)
                           surround piece = NestedPieces [pre', piece, post']
                           (result, cont) = formatItem s' f
                       in  (fmap surround result, cont)

formatItem ('{':s) f = let (arg, fspec:s') = matchBrace '{' s
                       in if fspec == '?'
                          then case format' arg f of
                                   Left  ps -> (Just $ SameLine "", s')
                                   Right ps -> (Just $ NestedPieces ps, s')
                          else (fchar fspec arg f, s')

formatItem (fspec:s) f = (fchar fspec "" f, s)
