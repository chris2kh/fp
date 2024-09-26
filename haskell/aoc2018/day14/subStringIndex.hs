

index :: String -> String -> Int -> Maybe Int
index [] _ n = Nothing
index _ [] _ = Nothing
index xs ys n = 
  case noFail xs ys of
   True  -> Just n
   False -> index xs (tail ys) (n+1)

noFail :: String -> String -> Bool
noFail [] _ = True
noFail _ [] = False
noFail (x:xs) (y:ys) = x == y && noFail xs ys

takeWhile1 :: (Char -> Bool) -> String -> String
takeWhile1 _ [] = []
takeWhile1 p (x:xs) | p x = x:(takeWhile1 p xs)
                    | otherwise = []

dropWhile1 :: (Char -> Bool) -> String -> String
dropWhile1 _ [] = []
dropWhile1 p (x:xs) | p x = dropWhile1 p xs
                    | otherwise = xs

isPrefixOf :: String -> String -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) | x == y = isPrefixOf xs ys
                         | otherwise = False

isSufixOf :: String -> String -> Bool
isSufixOf [] _ = True
isSufixOf _ [] = False
isSufixOf xs ys = xs == ys || isSufixOf xs (tail ys)


isInfixOf1 :: String -> String -> Bool
isInfixOf1 [] _ = True
isInfixOf1 _ [] = False
isInfixOf1 xs ys = isPrefixOf xs ys || isInfixOf1 xs (tail ys)




