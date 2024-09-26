{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

parseMessage :: String -> LogMessage
parseMessage m
 | length msgs < 3 = Unknown m
 | not $ first `elem` ["I","W","E"] = Unknown m
 | first == "I" = 
     LogMessage 
           Info 
           (read second::TimeStamp) 
           (unwords (drop 2 msgs))
 | first == "W" = 
     LogMessage 
           Warning 
           (read second::TimeStamp) 
           (unwords (drop 2 msgs))
 | first == "E" = 
     LogMessage 
           (Error (read second::Int)) 
           (read  (msgs !! 2)::TimeStamp) 
           (unwords (drop 3 msgs))
 | otherwise = Unknown m
 where
  msgs   = words m
  first  = head msgs
  second = msgs !! 1 
  
parse :: String -> [LogMessage]
parse =  (parseMessage <$>) . lines


insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert log Leaf = Node Leaf log Leaf
insert log (Node left curr right)
  | logTime <= currentTime =
       Node 
          (insert log left) 
          curr 
          right
  | otherwise = 
        Node 
          left 
          curr 
          (insert log right)
  where
    LogMessage _ logTime _ = log
    LogMessage _ currentTime _  = curr

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left current right) =
  (inOrder left) ++ [current] ++ (inOrder right)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong =
    (getMessage <$>)
    . (filter (isErrorEqualGreaterThan 50))
    . inOrder 
    . build

isErrorEqualGreaterThan :: Int -> LogMessage -> Bool
isErrorEqualGreaterThan 
      threshold 
      (LogMessage (Error x) _ _) = 
   x >= threshold
isErrorEqualGreaterThan _ _ = False

getMessage :: LogMessage -> String
getMessage (LogMessage _ _ m) = m
getMessage _ = ""

