module Transpose where

transpose :: [[Int]] -> [[Int]]
transpose xs | null $ head xs = []
transpose xs = [ head x | x <- xs] : transpose (tail <$> xs)

transpose2 :: [[Int]] -> [[Int]]
transpose2 [] = repeat []
transpose2 (x:xs) = zipWith (:) x (transpose2 xs)

