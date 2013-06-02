module FourChan.Helpers.String where

import Data.Char
import Data.Function
import Data.List
import Data.Monoid


instance Monoid Char where
    mempty = ' '
    mappend a b = chr (ord a + ord b)


splitList :: Eq a => [a] -> [a] -> ([a],[a])
splitList sample str =
    let (pre, post) = splitList' [] sample str
    in  (reverse pre, post)
    where
        splitList' :: Eq a =>  [a] -> [a] -> [a] -> ([a],[a])
        splitList' pre sample [] = (pre, [])
        splitList' pre sample post
            | sample `isPrefixOf` post = (pre, drop (length sample) post)
            | otherwise = splitList' (head post : pre) sample (tail post)

matchBrace :: Char -> String -> (String, String)
matchBrace open s = let (inner, next) = matchBrace' "" 1 s
                    in  (reverse inner, next)
    where
        matchBrace' :: String -> Int -> String -> (String, String)
        matchBrace' _ _ [] = error "Could not find matching brace"
        matchBrace' (_:content) 0 s = (content, s)
        matchBrace' content n (c:s) = matchBrace' (c:content) (n + weight c) s

        weight :: Char -> Int
        weight '{'
            | open == '{' = 1
            | otherwise = 0
        weight '}'
            | open == '{' = -1
            | otherwise = 0
        weight '['
            | open == '[' = 1
            | otherwise = 0
        weight ']'
            | open == '[' = -1
            | otherwise = 0
        weight '('
            | open == '(' = 1
            | otherwise = 0
        weight ')'
            | open == '(' = -1
            | otherwise = 0
        weight c = 0


data StringPiece = SameLine String
                 | Break
                 | MultiLine [String]
                 | NestedPieces [StringPiece]
                 deriving (Eq, Show)


piecesLines :: [StringPiece] -> [String]
piecesLines = map concat . concat . map (transpose . alignAll . flattenLine) . splitLines


alignAll :: [[String]] -> [[String]]
alignAll = alignCols . alignRows
    where
        alignRows = align
        alignCols = map align


align :: Monoid a => [[a]] -> [[a]]
align xss =
    let size = maximum . map length $ xss
    in  map (`expandTo` size) xss
    where
        expandTo :: Monoid a => [a] -> Int -> [a]
        ys `expandTo` n = ys ++ replicate (n - length ys) mempty

splitLines :: [StringPiece] -> [[StringPiece]]
splitLines = reverse . map reverse . splitLines' [] []
    where
        splitLines' :: [[StringPiece]] -> [StringPiece] -> [StringPiece]
                       -> [[StringPiece]]
        splitLines' result tmp [] = tmp : result
        splitLines' result tmp (Break : xs) = splitLines' (tmp:result) [] xs

        splitLines' result tmp (NestedPieces ys : xs) =
            let nestSplit = NestedPieces . map NestedPieces . splitLines
            in  splitLines' result (nestSplit ys : tmp) xs

        splitLines' result tmp (x:xs) = splitLines' result (x:tmp) xs

flattenLine :: [StringPiece] -> [[String]]
flattenLine = flattenLine'
    where
        flattenLine' :: [StringPiece] -> [[String]]
        flattenLine' [] = []
        flattenLine' (SameLine x : ps) = [x] : flattenLine' ps
        flattenLine' (MultiLine xs : ps) = xs : flattenLine' ps
        flattenLine' (NestedPieces nps : ps) = flattenLine' nps ++ flattenLine' ps
