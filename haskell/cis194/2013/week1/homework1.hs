toDigits :: Integer -> [Integer]
toDigits x
 | x <= 0 = []
 | otherwise = split x []
  where
   split 0 acc = acc
   split x acc = 
    split (x `quot` 10)
         ((x `mod` 10): acc)

doubleEvens :: (Integer, Integer) -> Integer
doubleEvens (pos, num)
 | pos `mod` 2 == 0 = num * 2
 | otherwise = num

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther nums =
 reverse $ doubleEvens <$> zip [1..] (reverse nums)

sumDigits :: [Integer] -> Integer
sumDigits nums =
  sum $ (sum . toDigits) <$> nums

validCreditCard :: Integer -> Bool
validCreditCard = 
 (== 0) . (`mod` 10) . sumDigits . doubleEveryOther. toDigits

---------------------------------
-- Hanoi towers
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c =
    (hanoi (n - 1) a c b) 
    ++ [(a, c)]
    ++ (hanoi (n - 1) b a c)
