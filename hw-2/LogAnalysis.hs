{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage str = let wordlist = words str in
                   case wordlist of
                     ("I":ts:msg) -> LogMessage Info (read ts) (unwords msg)
                     ("W":ts:msg) -> LogMessage Warning (read ts) (unwords msg)
                     ("E":lvl:ts:msg) -> LogMessage (Error (read lvl)) (read ts) (unwords msg)
                     _ -> Unknown (unwords wordlist)

parse :: String -> [LogMessage]
parse contents = map parseMessage (lines contents)


insert :: LogMessage -> MessageTree -> MessageTree
insert lmsg@(LogMessage _ _ _) Leaf = Node Leaf lmsg Leaf 
insert lmsg@(LogMessage _ ts _) (Node left lsmg2@(LogMessage _ ts2 _) right)
  | (ts > ts2) = Node left lsmg2 (insert lmsg right)
  | otherwise = Node (insert lmsg left) lsmg2 right 
insert _ t = t


build :: [LogMessage] -> MessageTree
build [] = Leaf
build (msg : msgs) = insert msg (build msgs)


inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) = (inOrder left) ++ [msg] ++ (inOrder right) 


whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong ((LogMessage (Error lvl) _ msg) : msgs) | (lvl >= 50) = [msg]  ++ (whatWentWrong msgs)
whatWentWrong (_: msgs) = whatWentWrong msgs

