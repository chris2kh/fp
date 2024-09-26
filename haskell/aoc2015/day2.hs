module Day2 where
import Utils
import Data.List

main :: IO ()
main = do
 input <- parseList "inputs/day2.txt"
 let orders = (( read <$>) . splitOn 'x') <$> input
 print $ sum $ paperNeeds  <$>  orders
 print $ sum $ ribbonNeeds <$>  orders

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn s xs = let
   before = takeWhile (/= s) xs
   rest   = dropWhile (/= s) xs
   after  = case rest of
            [] -> []
            r  -> splitOn s (tail r) 
   in before:after

paperNeeds :: [Int] -> Int
paperNeeds [l,w,h]  = let 
   a = l*w 
   b = w*h
   c = h*l
   in 2*(a+b+c) + minimum [a,b,c]

ribbonNeeds [l,w,h] = let
   a = l+w 
   b = w+h
   c = h+l
   in (l*w*h) + 2*minimum [a,b,c]
   

