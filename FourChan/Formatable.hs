module FourChan.Formatable where

import Control.Monad

import Data.Char
import Data.Either
import Debug.Trace (trace)


import FourChan.Helpers.String

class Formatable a where
    {-fhelp :: a -> [(Char,String)]-}
    fchar :: Char -> String -> a -> Maybe StringPiece


format :: Formatable a => String -> a -> [StringPiece]
format fmt f = either id id $ format' fmt f

format' :: Formatable a => String -> a -> Either [StringPiece] [StringPiece]
format' [] f = Right []
format' ('%':[]) f = error "Invalid character '%' at the end of the format string"
format' ('%':s) f = let (item, s') = formatItem s f
                    in case item of
                           Nothing -> Left $ format s' f
                           Just it -> fmap (it:) $ format' s' f
format' s f = let (plain, cont) = break (=='%') s
              in case format' cont f of
                     Left ps -> Left $ SameLine plain : ps
                     Right ps -> Right $ SameLine plain : ps

formatItem :: Formatable f => String -> f -> (Maybe StringPiece, String)
formatItem ('%':s) _ = (Just $ SameLine "%", s)
formatItem ('!':s) _ = (Just Break, s)

formatItem ('[':s) f = let (fill, s')     = matchBrace '[' s
                           (pre, post)    = splitList "%@" fill
                           (pre', post')  = (SameLine pre, SameLine post)
                           surround piece = NestedPieces [pre', piece, post']
                           (result, cont) = formatItem s' f
                       in  (fmap surround result, cont)

formatItem ('{':s) f = let (arg, fspec:s') = matchBrace '{' s
                       in if fspec == '?'
                          then case format' arg f of
                                   Left _ -> (Just $ SameLine "", s')
                                   Right ps -> (Just $ NestedPieces ps, s')
                          else (fchar fspec arg f, s')

formatItem (fspec:s) f = (fchar fspec "" f, s)
