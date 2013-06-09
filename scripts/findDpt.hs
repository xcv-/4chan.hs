#!/usr/bin/runghc

import FourChan

filters :: [(Thread -> String, String)]
filters = [ (safe getSubject,          "daily programming")
          , (safe getPlainTextComment, "working on")
          ]
    where safe get = maybe "" id . get . getOp

main :: IO ()
main = findThread "g" (Any filters) >>= mapM_ putStrLn . formatWithLinks "g"
