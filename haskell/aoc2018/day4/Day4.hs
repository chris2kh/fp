module Day4 where

import Text.ParserCombinators.Parsec hiding (count, State)
import Data.List (sort, maximumBy)
import Data.Map
import Data.Ord (comparing)
import Control.Monad.State

-- Parsing

data Record = Record 
   { date        :: Date
   , activity :: Activity
   } deriving (Show, Eq)

data Date = Date 
   { year    :: Int
   , month   :: Int
   , day     :: Int
   , hour    :: Int
   , minute  :: Int
   } deriving (Eq,Ord)

-- just to output sorted records and seeing them better
instance Show Date where
 show date =  "[" ++ 
              (show (year date)) ++ 
              "-" ++ 
              (show (month date)) ++ 
              "-" ++ 
              (show (day date)) ++ 
              " " ++ 
              (show (hour date)) ++ 
              ":" ++ 
              (show (minute date)) ++ 
              "]"

--instance Eq Date where
--  a == b =
--    year   a  ==  year  b &&
--    month  a  ==  month b &&
--    day    a  ==  day   b &&
--    hour   a  ==  hour  b &&
--    minute a  ==  minute b
--
--instance Ord Date where
--  a > b 
--   | year    a > year   b = True
--   | year    a < year   b = False
--
--   | month   a > month  b = True
--   | month   a < month  b = False
--
--   | day     a > day    b = True
--   | day     a < day    b = False
--
--   | hour    a > hour   b = True
--   | hour    a < hour   b = False
--
--   | minute  a > minute b = True
--   | minute  a < minute b = False
--
--   | otherwise = False
--
--  a <  b = not $ a > b 
--  a <= b = a < b || a == b
--
--instance Eq Record where
--  a == b = date a == date b
--
instance Ord Record where
  a <= b = date a <= date b


data Activity = 
         FallsAsleep
       | WakesUp
       | BeginsShift Int
 deriving (Show, Eq)

parseRecord :: Parser Record
parseRecord = do
   date     <- parseDate 
   activity <- parseActivity
   return $ Record date activity

parseDate :: Parser Date
parseDate = do
   char '['
   year <- integer
   char '-'
   month <- integer
   char '-'
   day <- integer
   space
   hour <- integer
   char ':'
   minute <- integer
   char ']'
   space
   return $ Date year month day hour minute  

parseActivity :: Parser Activity
parseActivity = parseFallsAsleep <|> parseWakesUp <|> parseBeginShift

parseFallsAsleep = do 
   s <- string "falls asleep"
   return FallsAsleep

parseWakesUp = do
  s <- string "wakes up"
  return WakesUp

parseBeginShift = do
  string "Guard #"
  guardId <- integer
  string " begins shift"
  return $ BeginsShift guardId

integer = read <$> many1 digit

-- Model
type GuardId = Int
type MinuteFallsAsleep = Int
-- remember this to compute sleep duration, time, etc
type CurrentGuardAndSleep = (GuardId, MinuteFallsAsleep)
-- initial state to start the process
initialGuardAndSleep = ((-1),0)
initialState = (empty, initialGuardAndSleep)

-- every guard will have an entry with one of these
-- in the dictionary
data SleepInfo = SleepInfo 
  { sleepCounter  :: Int
  , sleepRegistry :: Map Int Int
  } deriving Show


main :: IO ()
main = do
 contents <- parseFile "inputDay4.txt"
 let Right records         = traverse (parse parseRecord "") contents
 let (_, (dictionary, _))  = runState (track (sort records)) initialState
 -- strategy 1
 let guard1                = mostSleepGuard dictionary
 let minute1               = mostSleepMinute (dictionary ! guard1)
 putStrLn $ show (guard1 * minute1)
 -- strategy 2 
 let (guard2,minute2)      = strategy2 dictionary
 putStrLn $ show (guard2 * minute2)


strategy2 :: Map GuardId SleepInfo -> (GuardId, Int)
strategy2  =
    maximumBy (comparing snd)
   --for every entry in the list, map sleepInfo to mostSleepMinute of each guard
  . ((mostSleepMinute <$>) <$>) 
  . toList

mostSleepGuard :: Map GuardId SleepInfo -> GuardId
mostSleepGuard = 
     fst 
   . maximumBy (comparing snd)
   --for every entry in the list, map sleepInfo to SleepCounter
   . ((sleepCounter <$>) <$>) 
   . toList


mostSleepMinute :: SleepInfo -> Int
mostSleepMinute  = 
      fst 
    . (foldr1 myMax) 
    . toList 
    . sleepRegistry


track :: [Record] -> State (Map GuardId SleepInfo, CurrentGuardAndSleep ) ()
track [] = return ()
track (record:records) = do
  (dictionary, (guard, sleepTime)) <- get
  let now = (minute . date) record
  
  case (activity record) of
    BeginsShift next -> do
      put ((dictionary, (next,0)))
          --
    FallsAsleep ->
      put ((dictionary, (guard, now)))
   
    -- a lot of tracking when guard wakes up 
    WakesUp -> do
      put ((dictionary, (guard, now)))
            
      case (Data.Map.lookup guard dictionary) of
        -- first insert
        Nothing -> do
          let minutesZZZ    = now - sleepTime
          let registry      = registerSleepHours [sleepTime..(now-1)] empty
          let info          = SleepInfo minutesZZZ registry
          put ((insert guard info dictionary ), (guard, 0))
        -- update
        Just previous -> do
          let minutesZZZ    = (now - sleepTime) + sleepCounter previous
          let registry      = registerSleepHours [sleepTime..(now-1)]
                              (sleepRegistry previous)
          let info          = SleepInfo minutesZZZ registry
          put ((insert guard info dictionary), (guard, 0))
    -- keep tracking
  track records

  
registerSleepHours :: [Int] -> (Map Int Int) -> (Map Int Int)
registerSleepHours []     registry = registry
registerSleepHours (x:xs) registry =
  registerSleepHours xs (insertWith (+) x 1 registry)


-- for looking at the sorted data in a text editor
saveInFile :: String -> [Record] -> IO ()
saveInFile filename records= do
   writeFile filename $ unlines $ show <$> records


parseFile :: String -> IO [String] 
parseFile filename = readFile filename >>= ( return . lines)
