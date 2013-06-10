#!/usr/bin/runghc

import FourChan

filters :: [Filter Thread]
filters = [ Filter { getter = get getSubject
                   , pattern = "daily programming"
                   }
          , Filter { getter = get getPlainTextComment
                   , pattern = "working on"
                   }
          ]

    where get f = maybe "" id . f . getOp

main :: IO ()
main = findThread "g" (Any filters) >>= mapM_ putStrLn . formatWithLinks "g"
