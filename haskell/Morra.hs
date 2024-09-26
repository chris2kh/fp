module Morra where

import Control.Monad.State
import Control.Monad.Random

data Winner = Player1 | Player2 | None
type Fingers = Int
type Guess = Int
data Turn = Turn { fingers :: Fingers, guess :: Guess }

data Result = Result { numWinsP1 :: Int, numWinsP2 :: Int } deriving Show


type MyState = StateT Result IO Winner


roundWinner :: Turn -> Turn -> Winner
roundWinner player1 player2
          | guess player1 == result = Player1
          | guess player2 == result = Player2
          | otherwise = None
          where result = fingers player1 + fingers player2


playRound :: MyState
playRound = do
          result <- get
          if numWinsP1 result == 3
            then
              StateT $ \s -> do
                 putStrLn "player1 wins!"
                 print s
                 return (Player1, s)
            else do
             player <- StateT $ \s -> do
                 player <- round'
                 return (player, s)
             put (update result player)
             playRound 
          --if numWinsP1 results  == 3 
          -- then
          --     return $ do
          --        putStrLn "player1 wins!"
          --        return Player1
          -- else playRound


update :: Result -> Winner -> Result
update res Player1 = res { numWinsP1 = numWinsP1 res + 1 }
update res Player2 = res { numWinsP2 = numWinsP2 res + 1 }
update res None = res




main :: IO ()
main = putStrLn ""

round' :: IO Winner
round' = do
  p1 <- humanTurn
  p2 <- cpuTurn
  display "human" p1
  display "cpu" p2
  putStrLn $ "fingers : " ++ show (fingers p1 + fingers p2)
  case roundWinner p1 p2 of
    Player1 -> putStrLn "player1 won this round!" >> return Player1
    Player2 -> putStrLn "player2 won this round!" >> return Player2
    None    -> putStrLn "no winner in this round" >> return None
  
  
humanTurn :: IO Turn
humanTurn = do
   putStrLn "Choose the num of fingers (1 to 5)"
   fingers <- getLine
   putStrLn "Try to guess to sum of fingers (2 to 10)"
   guess <- getLine
   return $ Turn (read fingers) (read guess) 

cpuTurn :: IO Turn
cpuTurn = do 
         seed <- getStdGen 
         evalRandT (
              do
              fingers <- getRandomR (1, 5)
              guess  <-  getRandomR (2, 10)
              return $ Turn fingers guess
              ) seed

display :: String -> Turn -> IO ()
display player turn =
   putStrLn $ player ++ "--> fingers: " ++ (show $ fingers turn)
           ++ ", guess: " ++ (show $ guess turn)

