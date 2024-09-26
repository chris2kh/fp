module Day5 where
import Utils
import Data.List
import qualified Data.Set as S

main :: IO ()
main = do
 inputs <- parseList "inputs/day5.txt"

 -- Part a
 print $ length $ filter isNice1 <$> inputs

 -- Part b
 print $ length $ filter isNice2 <$> inputs

twoInRow :: String -> Bool
twoInRow [_] = False
twoInRow (x:y:xs) = x == y || twoInRow (y:xs) 


isNice1 :: String -> Bool
isNice1 xs =
       twoInRow xs
    && length (filter (`elem` "aeiou") xs) >= 3
    && flip all ["ab", "cd", "pq", "xy"] (not . (`isInfixOf` xs))


isNice2 :: String -> Ztring
isNice2 xs =
    nonOverlappingPairs xs && inBetween xs

nonOverlappingPairs xs =
  go xs S.empty
  where
   go [] _        = False
   go [_] _       = False
   
   go [x1,x2] set = let
    already = (x1,x2) `S.member` set in
    if already then True
               else False
   
   go (x1:x2:x3:xs) set = let
    already = (x1,x2) `S.member` set                  
    set' = S.insert (x1,x2) set in
    if already then True
               -- strings overlap, skip one character
               else if x1 == x2 && x2 == x3 then go (x3:xs) set'
                                            else go (x2:x3:xs) set'

inBetween = go
  where
   go [] = False
   go [_,_] = False
   go (x1:x2:x3:xs) | x1 == x3 = True
                    | otherwise = go (x2:x3:xs) 
      
