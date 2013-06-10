module FourChan.Format.Formatable
( Formatable
, fchar
, fcharError
, format
) where

import Control.Applicative
import Control.Monad

import Control.Monad.Reader
import Control.Monad.State

import Data.Char
import Data.Either
import Data.List

import Text.Printf

import FourChan.Format.StringPiece


class Formatable a where
    {-fhelp :: a -> [(Char,String)]-}
    fchar :: (Applicative m, Monad m) => Char -> String -> a -> m StringPiece
    fcharError :: Monad m => Char -> String -> a -> m StringPiece
    fcharError c arg = fail $
        printf "Unknown format specifier '%c' with argument \"%s\"" c arg



data MayFail a = Correct a | Failed a

ignoreFail :: MayFail a -> a
ignoreFail (Correct x) = x
ignoreFail (Failed x) = x


instance Functor MayFail where
    fmap f (Correct a) = Correct (f a)
    fmap f (Failed a) = Failed (f a)

instance Applicative MayFail where
    pure = Correct
    (Correct f) <*> (Correct x) = Correct (f x)
    (Failed f)  <*> (Correct x) = Failed (f x)
    (Correct f) <*> (Failed x)  = Failed (f x)
    (Failed f)  <*> (Failed x)  = Failed (f x)

instance Monad MayFail where
    return = Correct
    (Correct x) >>= f = f x
    (Failed x)  >>= f = case f x of
                            Correct y -> Failed y
                            Failed y  -> Failed y



newtype Formatter f = Formatter { runFormatter :: StateT String (ReaderT f MayFail) [StringPiece] }


format :: Formatable f => String -> f -> [StringPiece]
format fmt f = ignoreFail $ format' fmt f


format' :: Formatable a => String -> a -> MayFail [StringPiece]
format' [] f = return []
format' ('%':[]) f = error "Invalid character '%' at the end of the format string"
format' ('%':s) f = let (item, s') = formatItem s f
                    in case item of
                           Nothing -> Failed $ format s' f
                           Just it -> fmap (it:) $ format' s' f

format' s f = let (plain, cont) = break (=='%') s
              in fmap (SameLine plain :) $ format' cont f


formatItem :: (Formatable f, Applicative m, Monad m) =>
              String -> f -> (m StringPiece, String)

formatItem ('%':s) _ = (return $ SameLine "%", s)
formatItem ('!':s) _ = (return Break, s)

formatItem ('(':s) f = let (sep, s') = matchBrace '(' s
                           fmtSep    = NestedPieces $ format sep f
                           (x, s'')  = formatItem s' f
                       in (x >>= formatNested fmtSep, s'')
    where
        formatNested :: Monad m => StringPiece -> StringPiece -> m StringPiece
        formatNested sep (NestedPieces ps) = let ps' = intersperse sep ps
                                             in return $ NestedPieces ps'
        formatNested _ _ = fail "Unknown piecewise format specifier with () block"

formatItem ('[':s) f = let (fill, s')     = matchBrace '[' s
                           (pre, post)    = splitList "%@" fill
                           (pre', post')  = (Column pre, Column post)
                           surround piece = NestedPieces [pre', piece, post']
                           (result, cont) = formatItem s' f
                       in  (fmap surround result, cont)

formatItem ('{':s) f = let (arg, fspec:s') = matchBrace '{' s
                       in if fspec == '?'
                          then case format' arg f of
                                   Failed  ps -> (return $ SameLine "", s')
                                   Correct ps -> (return $ NestedPieces ps, s')
                          else (fchar fspec arg f, s')

formatItem (fspec:s) f = (fchar fspec "" f, s)
