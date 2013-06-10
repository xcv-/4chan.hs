module FourChan.Helpers.Html
( plainText
) where

import Text.HTML.TagSoup


plainText :: String -> String
plainText = innerText . map endlines . parseTags
    where
        endlines (TagOpen "br" _) = TagText "\n"
        endlines tag = tag
