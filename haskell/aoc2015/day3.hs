{-# LANGUAGE LambdaCase #-}
module Day3 where
import Prelude hiding (odd, even) 
import Utils
import qualified Data.Set as S

main :: IO ()
main = do
  input <- parseString "inputs/day3.txt"

  -- answer part a
  print $ S.size $  iter (S.insert (0,0) S.empty) (0,0) input

  -- answer part b
  let santaRoutes = iter (S.insert (0,0) S.empty) (0,0) $ odd input 
  let robotRoutes = iter (S.insert (0,0) S.empty) (0,0) $ even input 
  print $ S.size $ S.union santaRoutes robotRoutes



iter :: (S.Set (Int, Int)) 
     -> (Int, Int) 
     -> String 
     -> (S.Set (Int, Int))

iter visits _ [] = visits
iter visits current (x:xs) = let
       next      = advance current x
       updated   = S.insert next visits
       in iter updated next xs

advance :: (Int, Int) -> Char -> (Int, Int)
advance (row,col) = \case
   '^' -> (row, col + 1) 
   'v' -> (row, col - 1) 
   '<' -> (row - 1, col) 
   '>' -> (row + 1, col) 

odd :: String -> String
odd = \case
  []            -> []
  [x]           -> [x]
  (x1:_:xs) -> x1:odd xs

even :: String -> String
even = \case
  []            -> []
  [x]           -> []
  (_:x2:xs) -> x2:even xs

