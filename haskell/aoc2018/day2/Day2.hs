module Day2.Day2 (parse) where

import Data.Map
  ( Map
  , empty
  , elems
  , insertWith
  , findWithDefault
  )
import Data.List (sort)
import Control.Monad.State

main :: IO ()
main = do
  boxIds <- parse "inputDay2.txt"
  let (count2s, count3s) = execState (count boxIds) (0,0)
  putStrLn $ "the answer for problem 1 is: " 
         ++ (show $ count2s * count3s)
 
  putStrLn $ "the answer for problem2 is: "
        ++ (show . commonChars . similar) boxIds


parse :: String -> IO [String] 
parse filename = readFile filename >>= ( return . lines)


freqs :: String -> [Int]
freqs = elems . foldr upsert empty 
 where 
  upsert char map =
   insertWith (+) char 1 map

type Count_2_3 = (Int, Int)

count :: [String] -> State Count_2_3 ()
count [] = return ()
count (x:xs) = do
  let local = countLocal $ freqs x
  modify (\global -> (fst global + fst local, snd global + snd local))
  count xs

countLocal = go (0,0)
 where
  go res     []     = res
  go (1,1)   _      = (1,1)
  go (x2,x3) (x:xs)
       | x >= 3     = go (x2, 1) xs
       | x == 2     = go (1, x3) xs
       | otherwise  = go (x2,x3) xs


similar :: [String] -> Maybe [String]
similar = find . sort where
   find []         = Nothing
   find [x]        = Nothing
   find (x1:x2:xs) | differentChars x1 x2 == 1 = Just [x1,x2]
                   | otherwise = find (x2:xs)
   
   differentChars [] []         = 0
   differentChars x  []         = length x
   differentChars [] y          = length y
   differentChars (x:xs) (y:ys) | x == y    =     differentChars xs ys
                                | otherwise = 1 + differentChars xs ys
                           
commonChars :: Maybe [String] -> String
commonChars Nothing       = "no common boxes :("
commonChars (Just [x, y]) = go x y
     where go [] _ = []
           go _ [] = []
           go (x:xs) (y:ys) | x == y    = x: go xs ys
                            | otherwise =    go xs ys




