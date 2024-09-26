import Test.QuickCheck

merge :: Ord a => [a] -> [a] -> [a]
merge (x:xs) (y:ys)
  | x < y     = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys
merge [] ys = ys
merge xs [] = xs

prop_numElements :: [Integer] -> [Integer] -> Bool
prop_numElements xs ys
  = length xs + length ys == length (merge xs ys)

prop_sorted :: OrderedList Integer -> OrderedList Integer -> Bool
prop_sorted (Ordered xs) (Ordered ys)
  = merge xs ys == sort (xs ++ ys)

sort :: [Integer] -> [Integer]
sort [] = []
sort (x:xs) = 
    sort ([left | left <- xs, left <= x  ]) ++ [x] ++ 
    sort ([right | right <- xs, right > x  ])


main = do
  quickCheck prop_sorted
