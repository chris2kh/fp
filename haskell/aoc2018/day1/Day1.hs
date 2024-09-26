module Day1 where

import Test.Hspec
import Data.Set (insert, member, empty, Set)


main :: IO ()
main = do
  frequencies <- parse "inputDay1.txt"
  
  putStrLn $ "the answer for part 1 is :" 
   ++ show (sum frequencies)
  
  putStrLn $ "the answer for part 2 is : " 
   ++ show (findFirstRepeatSum 0 frequencies)


parse :: String -> IO [Int]
parse filename = do
   content <- readFile filename
   return $ parseNum <$> lines content

parseNum :: String -> Int
parseNum num@('-': _) = read num
parseNum num@('+': x) = read x


data State a = State 
  { firstRep    :: Maybe a
  , seenFreqs   :: Set a
  , freq        :: a
  } deriving Show


findFirstRepeatSum :: Int -> [Int] -> Int
findFirstRepeatSum initial frequencies =
   answer where
    
    state = State 
             { firstRep  = Nothing
             , seenFreqs = insert initial empty
             , freq      = initial
             }
   
    State {firstRep = Just answer } = 
        until' state isFirstRepeat updateFreq (cycle' frequencies)
   
cycle' :: [a] -> [a]
cycle' [] = error "empty list"
cycle' xs = go xs
 where go [] = go xs   
       go (x:xs) = x : go xs

until' :: State a -> 
         (State a -> Bool) -> 
         (a -> State a -> State a) ->
         [a] -> 
         State a

until' state _ _ [] = state
until' state checkCondition update (x:xs) 
      | checkCondition state = state
      | otherwise = until' newState checkCondition update xs
      where newState = update x state

isFirstRepeat :: (State Int) -> Bool
isFirstRepeat s = case firstRep s of
            Nothing -> False
            _       -> True

updateFreq :: Int -> State Int -> State Int
updateFreq delta s 
   | freq' `member` seenFreqs s =
     s { firstRep = Just freq', freq = freq' }
   | otherwise =
     s { seenFreqs = insert freq' (seenFreqs s), freq = freq' }
   where freq' = freq s + delta  

test :: IO ()
test = hspec $ do
 describe "parse Num" $ do
   
   it "should parse +1" $ do
     parseNum "+1" `shouldBe` 1
   
   it "should parse +2" $ do
     parseNum "+2" `shouldBe` 2

   it "should parse -4" $ do
     parseNum "-4" `shouldBe` (-4)

 describe "parse File" $ do
   it "should parse test1.txt" $ do
    frequencies <- parse "test1.txt"
    frequencies `shouldBe` [3,3,4,(-2),(-4)] 

   it "should parse test2.txt" $ do
    frequencies <- parse "test2.txt"
    frequencies `shouldBe` [1,(-2),3,1,1,(-2)] 

   it "should parse test3.txt" $ do
    frequencies <- parse "test3.txt"
    frequencies `shouldBe` [(-6),3,8,5,(-6)] 
   
   it "should parse test4.txt" $ do
    frequencies <- parse "test4.txt"
    frequencies `shouldBe` [7,7,(-2),(-7),(-4)] 
   
 describe "find first repeated frequency" $ do
   it "should find first repeated freq 1" $ do
    findFirstRepeatSum 0 [3,3,4,(-2),(-4)] `shouldBe` 10
   
   it "should find first repeated freq 2" $ do
    findFirstRepeatSum 0 [1,(-2),3,1,1,(-2)] `shouldBe` 2
 
   it "should find first repeated freq 3" $ do
    findFirstRepeatSum 0 [(-6),3,8,5,(-6)] `shouldBe` 5
    
   it "should find first repeated freq 4" $ do
    findFirstRepeatSum 0 [7,7,(-2),(-7),(-4)] `shouldBe` 14 
