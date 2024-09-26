{-# LANGUAGE NumericUnderscores #-}
module Day12 where

import qualified Data.Map as M
import Text.ParserCombinators.Parsec

parseFile :: String -> IO [String]
parseFile filename =
  readFile filename >>= (return . lines)

parseInitialState :: Parser String
parseInitialState = do
  string "initial state: "
  state <- many (char '#' <|> char '.')
  return state

parseTransitions :: Parser (String, Char)
parseTransitions = do
 plants <- many (char '#' <|> char '.')
 string " => "
 result <- (char '#' <|> char '.')
 return (plants, result)

main :: IO ()
main = do
 line1:_:rest <- parseFile "day12_input.txt"
 let Right initialState = (parse parseInitialState "") line1
 let Right transitions = traverse (parse parseTransitions "") rest
 print transitions
 let map = foldl (\map (k,v) -> M.insert k v map) M.empty transitions
 let state = (take 1000 $ repeat '.') ++ initialState ++ 
             (take 1000 $ repeat '.')
 generation 0 map state

generation :: Int -> M.Map String Char -> String -> IO ()
generation n map state = do
      print $ show n ++ ": " ++ show (result state)
      --print $ show n ++ (if n < 10 then ":  " else ": ") ++ state
      if n == 200
        then do
         return ()
        else generation (n+1) map (transit map state)


result =
   sum
 . map fst
 . filter (\ (num, char) -> char == '#')
 . zip [-1000..]


transit :: M.Map String Char -> String -> String
transit map list@(x1:x2:xs) = 
  go (last' 2 list ++ list ++ take 2 list)
  where
    go (x1:x2:x3:x4:x5:[]) =
      (M.findWithDefault '.' (x1:x2:x3:x4:x5:[]) map) :[]
    go (x1:x2:x3:x4:x5:xs) =
      (M.findWithDefault '.' (x1:x2:x3:x4:x5:[]) map): go (x2:x3:x4:x5:xs) 

last' :: Int -> [a] -> [a]
last' n l | n >= len = l 
          | otherwise = drop (len-n) l
          where len = length l
