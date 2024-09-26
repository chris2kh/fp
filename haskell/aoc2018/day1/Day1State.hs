
module Day1 (parse) where

import Test.Hspec
import Data.Set (Set,insert, member, empty) 
import Control.Monad.State


main :: IO ()
main = do
  frequencies <- parse "inputDay1.txt"
  
  putStrLn $ "the answer for part 1 is :" 
   ++ show (sum frequencies)
  
  putStrLn $ "the answer for part 2 is : " 
   ++ show (findFirstRepeatFreq 0 frequencies)


parse :: String -> IO [Int]
parse filename = do
   content <- readFile filename
   return $ parseNum <$> lines content

parseNum :: String -> Int
parseNum num@('-': _) = read num
parseNum num@('+': x) = read x


findFirstRepeatFreq :: Int -> [Int] -> Int
findFirstRepeatFreq initial frequencies =
  answer 
  where
  Just answer = evalState (findIn frequencies) 
                       (insert initial empty, 0)


  findIn :: [Int] -> State (Set Int, Int) (Maybe Int)
  findIn [] = findIn frequencies
  findIn (x:xs) = do 
   
   (alreadySeenResults, result ) <- get
   let newResult = x + result
   
   if newResult `member` alreadySeenResults
    then return $ Just newResult
    else do 
     put (insert newResult alreadySeenResults, newResult)
     findIn xs


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
    findFirstRepeatFreq 0 [3,3,4,(-2),(-4)] `shouldBe` 10
   
   it "should find first repeated freq 2" $ do
    findFirstRepeatFreq 0 [1,(-2),3,1,1,(-2)] `shouldBe` 2
 
   it "should find first repeated freq 3" $ do
    findFirstRepeatFreq 0 [(-6),3,8,5,(-6)] `shouldBe` 5
    
   it "should find first repeated freq 4" $ do
    findFirstRepeatFreq 0 [7,7,(-2),(-7),(-4)] `shouldBe` 14 
