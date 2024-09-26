import Data.List

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
 | even x = (x - 2) * fun1 xs
 | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (+ (-2)) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3* n + 1)

fun2' :: Integer -> Integer
fun2' = sum 
      . filter even 
      . takeWhile (>1) 
      . iterate (\x -> if even x then x `div` 2 else 3 * x + 1) 

-----------------------

data Tree a =
     Leaf
   | Node Integer (Tree a) a (Tree a)
   deriving (Show, Eq)

getHeight :: Tree a -> Integer
getHeight Leaf = (-1)
getHeight (Node h _ _ _) = h

myInsert :: a -> Tree a -> Tree a
myInsert x Leaf = Node 0 Leaf x Leaf
myInsert x (Node h l curr r)
  | hLeft <= hRight = Node (max (getHeight newL) hRight + 1) newL curr r
  | hLeft > hRight = Node (max hLeft (getHeight newR) + 1) l curr newR
  where
   hLeft  = getHeight l
   hRight = getHeight r
   newL = myInsert x l
   newR = myInsert x r

foldTree :: [a] -> Tree a
foldTree = foldr myInsert Leaf

------
-- Exercise 2
xor :: [Bool] -> Bool
xor = odd . foldr (\x acc -> acc + if x == True then 1 else 0) 0

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc)  [] 

--------
-- Exercise 3
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl f z = foldr (flip f) z . reverse

-----
-- Exercise 4
sieve :: Integer -> [Integer] 
sieve n = filter (not . (`elem` sundaramNums)) [1..n] where
  sundaramNums = 
        [ val | x <- [1..n], y <- [1..x],
        let val =  x + y + 2*x*y, val <= n]

sieve' :: Integer -> [Integer]
sieve' n = (+ 1) . (* 2) <$>[1..n] \\ sundarumNums
 where 
 sundarumNums = 
  [ val | x <- [1..n], y <- [1..x],
  let val =  x + y + 2*x*y, val <= n]
