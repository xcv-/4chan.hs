module FourChan.Helpers.StringPiece where

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
                 | Column String
                 | Row String
                 deriving (Eq, Show)


piecesLines :: [StringPiece] -> [String]
piecesLines ps =
    let splitted = splitLines $ flatten ps
    in do
        line <- splitted
        let aligned = map (toStrings . alignColumn) $ alignRows line
        map concat $ transpose $ aligned


toStrings :: StringPiece -> [String]
toStrings (SameLine x) = [x]
toStrings (MultiLine xs) = xs


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


flatten :: [StringPiece] -> [StringPiece]
flatten [] = []
flatten (NestedPieces ps' : ps) = flatten ps' ++ flatten ps
flatten (p:ps) = p : flatten ps


height :: StringPiece -> Int
height (SameLine _) = 1
height Break = 2
height (MultiLine xs) = length xs
height (NestedPieces ps) = maximum $ map height ps
height (Column _) = 0


expandTo :: StringPiece -> Int -> StringPiece
(SameLine x)   `expandTo` h = MultiLine $ x : replicate (h -1) ""
(MultiLine xs) `expandTo` h = MultiLine $ xs ++ replicate (h - length xs) ""
(Column x)     `expandTo` h = MultiLine $ replicate h x


alignRows :: [StringPiece] -> [StringPiece]
alignRows ps =
    let maxHeight = maximum $ map height ps
    in  map (`expandTo` maxHeight) ps


alignColumn :: StringPiece -> StringPiece
alignColumn p@(SameLine _) = p
alignColumn p@(MultiLine xs) =
    let maxWidth = maximum $ map length xs
    in  MultiLine $ map (\l -> l ++ replicate (maxWidth - length l) ' ') xs

