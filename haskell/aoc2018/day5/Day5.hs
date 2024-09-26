module Day5 where

import Data.Char

main :: IO ()
main = do
 contents <- parseFile "inputDay5.txt"
 putStrLn $ show $ length $ react contents
 --writeFile "resultDay5_2.txt" $ reverse $ react contents
 putStrLn $ show $ shortestPolymer contents

parseFile :: String -> IO String
parseFile = readFile


shortestPolymer :: String -> Int
shortestPolymer xs = 
   foldr1 min $ (length . react . flip remove xs) <$>  ['a'..'z']






xor :: Bool -> Bool -> Bool
xor False x     = x
xor x     False = x
xor True  x     = not x

react :: String -> String
react = go []
 where 
  go :: Stack -> String -> String
  go stack [] = stack
  go stack (x:xs) =
   case peek stack of
    Nothing  -> go (push x stack) xs
    (Just y) -> if shouldReact x y 
                 then go (pop stack) xs
                 else go (push x stack) xs 

react2 :: String -> String
react2 = go False []
 where
  go again acc [] | again = go False [] acc
                  | otherwise = acc
  go again acc (x1:x2:xs) 
                  | shouldReact x1 x2 = go True acc xs
                  | otherwise = go again (x1:acc) (x2:xs)
  go again acc (x:xs) = go again (x:acc) xs 


shouldReact :: Char -> Char -> Bool
shouldReact x1 x2 = (toUpper x1 == x2) `xor` (x1 == toUpper x2) 


type Stack = [Char]
peek :: Stack -> Maybe Char
peek [] = Nothing
peek (x:_) = Just x

pop :: Stack -> Stack
pop [] = []
pop (_:xs) = xs

push :: Char -> Stack -> Stack
push x xs = x:xs


remove :: Char -> String -> String
remove _ [] = []
remove c (x:xs) | x == c || toUpper c == x = remove c xs
                | otherwise = x:(remove c xs)



