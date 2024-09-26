module Cristian where
 import Data.Char

-- Exercise 1.1

 half :: Integer -> Integer
 half x = x `div` 2

--- F# pipe operator is just a function and we can define it like this:
 (|>) :: (a -> b) -> (b -> c) -> a -> c
 (|>) f g a = g $ f a

 halveEvensF_sharp:: [Integer] -> [Integer]
 halveEvensF_sharp = 
  filter even |>
  map half


 halveEvens:: [Integer] -> [Integer]
 halveEvens = 
  map half . filter even

-- Exercise 1.2

 safeCharacter:: Char -> Char
 safeCharacter c
  | not (isControl c) &&
    isAlphaNum c = c
  | otherwise = '_'


 safeString :: String -> String
 safeString = map safeCharacter 

-- Exercise 1.3

 noChar :: String -> Integer -> String
 noChar (x:xs) 0 = xs
 noChar (x:xs) n = x: noChar xs (n-1)

 missingChar:: String -> [String]
 missingChar xs = map (noChar xs) [0..fromIntegral $ length xs - 1]


-- Exercise 1.4
 compare' :: Ord a => a -> a -> a
 compare' x y
  | x > y = x
  | otherwise = y

 longestText :: Ord a => [a] -> a
 longestText xs = foldl compare' (xs !! 0) xs

-- Exercise 1.5
 adjacents :: [a] -> [(a,a)]
 adjacents [] = []
 adjacents [_] = []
 adjacents (x:xs) =
  (x, xs !! 0): adjacents xs

-- Exercise 1.6
 commas :: [String] -> String
 commas [] = ""
 commas [x] = x
 commas (x:xs) =
  x ++ ", " ++ commas xs

 commas2 :: [String] -> String
 commas2 [] = ""
 commas2 [x] = x
 commas2 xs =
   result where
    _:_:result =
        foldl (\acc x -> acc ++", " ++ x) "" xs

-- Exercise 1.7
 sumL :: [Integer] -> [Integer] -> [Integer]
 sumL [] [] = []
 sumL (x:xs) (y:ys) =
  (x+y): sumL xs ys

 addPoly :: [[Integer]] -> [Integer]
 addPoly (x:xs) =
  foldl sumL x xs

-- Exercise 1.8
 f:: Integer -> String -> String -> Integer 
 f sum "0" "" = sum
 f sum seq "" = sum + read seq
 f sum seq (x:xs)
  | isDigit $ x =
       -- update the digit sequence
       f sum (seq ++ [x]) xs
  | seq /=  "0" = 
       -- update the sum and reset sequence
       f (sum + read seq) "0" xs 
  | otherwise =
       -- nothing to update, continue
       f sum seq xs     

 sumNumbers :: String -> Integer
 sumNumbers xs =
  f 0 "0" xs 

-- Exercise 2
 numLines :: String -> Integer
 numLines  = fromIntegral .length . lines

 emptyLines :: String -> Integer
 emptyLines = fromIntegral. length . filter (== "") . lines
 
 numWords :: String -> Integer
 numWords = fromIntegral. length . words


 lenLongestLine :: String -> Integer
 lenLongestLine =
      maxL . map numWords . lines
       where maxL = fromIntegral .foldl1 max

 follow :: String -> Integer
 follow  xs =
  follow'$ words xs where
     follow' :: [String] -> Integer
     follow' [] = 0
     follow' (x:xs) 
        | xs == [] = 0
        | otherwise =
           (if x == xs!!0 then 1 else 0) + follow' xs 

 makeStat :: String ->  (String, (String -> Integer)) -> String
 makeStat xs (label, f) =
    label ++ ": " ++ show (f xs)
 
 wordCount :: String -> [String]
 wordCount xs =
    map (makeStat xs)
      [ ("Number of lines", numLines)
      , ("Number of empty lines", emptyLines)
      , ("Number of words", numWords)
      , ("Number of words followed by themselves",  follow)
      , ("Length of the longest line", lenLongestLine)
      ] 

-- Unit testing
 ex_halveEvens =
    [ halveEvens [] == []
    , halveEvens [1,2,3,4,5] == [1,2]
    , halveEvens [6,6,6,3,3,3,2,2,2] == [3,3,3,1,1,1]
    ]

 ex_safeString =
    [ safeString [] == []
    , safeString "Hello World!" == "Hello World!"
    , safeString "That’s your line:\n" == "That_s your line:_"
    , safeString "🙋.o(“Me Me Me”)" == "_.o(_Me Me Me_)"
    ]
 
 ex_missingChar =
   [ missingChar "" == []
   , missingChar "Hello" == ["ello", "Hllo", "Helo", "Helo", "Hell"]
   ]

 ex_longestText =
   [ longestText [True,False] == False
   , longestText [2,4,16,32] == (32::Int)
   , longestText (words "Hello World") == "World"
   , longestText (words "Olá mundo") ==  "Olá"
   ]

 ex_adjacents =
   [ adjacents "" == []
   , adjacents [True] == []
   , adjacents "Hello" == [('H','e'),('e','l'),('l','l'),('l','o')]
   ]

 ex_commas =
   [ commas [] == ""
   , commas ["Hello"] == "Hello"
   , commas ["Hello", "World"] == "Hello, World"
   , commas ["Hello", "", "World"] == "Hello, , World"
   , commas ["Hello", "new", "World"] == "Hello, new, World"
   ]

 ex_addPoly =
   [ addPoly [[]] == []
   , addPoly [[0, 1], [1, 1]] == [1, 2]
   , addPoly [[0, 1, 5], [7, 0, 0], [-2, -1, 5]] == [5, 0, 10]
   ]

 ex_sumNumbers =
   [ sumNumbers "" == 0
   , sumNumbers "Hello world!" == 0
   , sumNumbers "a1bc222d3f44" == 270
   , sumNumbers "words0are1234separated12by3integers45678" == 46927
   , sumNumbers "000a." == 0
   , sumNumbers "0.00a." == 0
   ]
   
 testResults :: [(String, [Bool])]
 testResults = 
     [ ("halveEvens", ex_halveEvens)
     , ("safeString", ex_safeString)
     , ("missingChar", ex_missingChar)
     , ("longestText", ex_longestText)
     , ("adjacents", ex_adjacents)
     , ("commas", ex_commas)
     , ("addPoly", ex_addPoly)
     , ("sumNumbers", ex_sumNumbers)
     ]

 makeReport :: [Bool] -> String
 makeReport [] = "Empty test suite"
 makeReport xs
   | all == fail =
        "All " ++ (show fail) ++ "tests failed"
   | all == pass =
        successfulTextMsg
   | otherwise =
        successfulTextMsg ++ ". " ++ failMsg
   where 
     all = length xs
     pass = sum $ map fromEnum xs
     fail = all - pass
     successfulTextMsg =
        (show pass) ++ "/" ++ (show all) ++ " successful tests"
     failMsg =
        "Failing tests: " ++ "TODO" 

 failingTests :: [Bool] -> String
 failingTests xs =
   -- incompleto, vim se me cerro y se perdio
   -- el trabajo, pero aca iba un composicion
   -- de funciones para sacar lo que necesitamos
   zip xs [1..length xs]
        
 
 format :: (String, [Bool]) -> String
 format (label, results) =
      label ++ ": " ++ makeReport results
      
 formatTests :: [(String, [Bool])] -> String
 formatTests = map format

