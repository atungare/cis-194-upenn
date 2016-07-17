{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- E1
parseMessage :: String -> LogMessage
parseMessage s = case words s of
  ("E":e:t:msg) -> LogMessage (Error (read e)) (read t) (unwords msg)
  ("I":t:msg) -> LogMessage Info (read t) (unwords msg)
  ("W":t:msg) -> LogMessage Info (read t) (unwords msg)
  _ -> Unknown s

parse :: String -> [LogMessage]
parse = (map parseMessage) . lines

