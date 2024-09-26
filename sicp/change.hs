change :: Int -> [Int] -> Int
change ammount denominations
  | null denominations || ammount < 0 = 0
  | ammount == 0 = 1
  | otherwise = 
    (change ammount (tail denominations)) +
    (change (ammount - head denominations) denominations)