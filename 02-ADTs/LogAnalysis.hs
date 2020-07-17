{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage s = case (words s) of
  ("I":ts:body) -> LogMessage Info (read ts::Int) (unwords body)
  ("W":ts:body) -> LogMessage Warning (read ts::Int) (unwords body)
  ("E":level:ts:body) -> LogMessage (Error (read level::Int)) (read ts::Int) (unwords body)
  _ -> Unknown s

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert newMessage@(LogMessage _ ts _) tree = case tree of
  Leaf -> Node Leaf newMessage Leaf
  Node left message@(Unknown _) right -> Node (insert newMessage left) message right
  Node left message@(LogMessage _ curr _) right
    | ts < curr -> Node (insert newMessage left) message right
    | otherwise -> Node left message (insert newMessage right)

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left message right) = (inOrder left) ++ (message : (inOrder right))

isSevere :: Int -> LogMessage -> Bool
isSevere minLevel (LogMessage (Error level) _ _) = level >= minLevel
isSevere _ _ = False

getBody :: LogMessage -> String
getBody (LogMessage _ _ body) = body
getBody _ = ""

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map getBody . inOrder . build . filter (isSevere 50)
