import Data.List

fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

findFibWith substr = find (isInfixOf substr . show) fibs

partA n =
 take 8 (show result)
  where
   firstFibs = take n fibs >>= show 
   Just result = findFibWith firstFibs
