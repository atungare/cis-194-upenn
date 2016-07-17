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

-- E2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert m@(LogMessage _ t _) tree = case tree of
  (Leaf) -> Node Leaf m Leaf
  (Node l msg@(LogMessage _ ot _) r) ->
    if t > ot
      then (Node l msg (insert m r))
      else (Node (insert m l) msg r)

-- E3
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

-- E4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l m r) = (inOrder l) ++ [m] ++ (inOrder r)

-- E5
isErrorGT50 :: LogMessage -> Bool
isErrorGT50 (LogMessage (Error e) _ _) =
  if e >= 50 then True else False
isErrorGT50 _ = False

msgString :: LogMessage -> String
msgString (LogMessage _ _ msg) = msg

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = (map msgString) . inOrder . build . (filter isErrorGT50)

