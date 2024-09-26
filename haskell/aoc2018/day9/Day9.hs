{-# LANGUAGE BangPatterns #-} 
module Day9 where

import Data.List
import qualified Data.Map as M

--10 players; last marble is worth 1618 points: high score is 8317
--13 players; last marble is worth 7999 points: high score is 146373
--17 players; last marble is worth 1104 points: high score is 2764
--21 players; last marble is worth 6111 points: high score is 54718
--30 players; last marble is worth 5807 points: high score is 37305
--471 players; last marble is worth 72026 points

numPlayers = 471
numRounds = 72026


main :: IO ()
main = do
 let dict  = M.fromList $ zip [1..numPlayers] (repeat 0)
 res <- play 1 1 [0] 0 dict
 print $ maximum $ snd <$> M.toList res


play :: Int -> Int ->  [Int]  -> Int -> (M.Map Int Int) -> IO (M.Map Int Int)
play  _ n _  _ dict | n > numRounds = return dict
play player marble marbles focus dict = do

  if (marble `mod` 23 == 0) then do
   let focus'   = shiftDown 7 focus marbles
   let !score   = marble + (marbles !! focus')
   let dict'    = M.insertWith (+) player score dict
   let marbles' = remove marbles focus'
   play (next player) (marble + 1) marbles' focus', dict'  

  else do 
   let (focus', marbles') = add marble marbles focus 
   play (next player) (marble + 1) marbles' focus', dict'  
   

shiftDown :: Int-> Int -> [Int] -> Int
shiftDown amount focus marbles 
     | focus >= amount = focus - amount
     | otherwise       = len - (amount - focus)                  
     where len  = length marbles
                       
next :: Int -> Int
next p = (p `mod` numPlayers) + 1

add :: Int -> [Int] -> Int -> (Int, [Int])
add marble marbles focus
   | focus == len - 1 = (1,   0:marble:(drop 1 marbles))
   | focus == len - 2 = (len, marbles ++ [marble])
   | otherwise        = 
                        (focus + 2,
                           (take (focus + 2) marbles) ++ 
                           [marble]                  ++
                           (drop (focus + 2) marbles) 
                        )
   where len = length marbles

remove :: [Int] -> Int -> [Int]
remove marbles pos = (take pos marbles) ++ (drop (pos + 1) marbles) 
