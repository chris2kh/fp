module Day10 where

import Control.Monad
import Data.Set hiding (take)
import Text.ParserCombinators.Parsec

parseFile :: String -> IO [String]
parseFile  filename =
   readFile filename >>= (return . lines)

parsePoint :: Parser Point
parsePoint = do
  string "position=<"
  many space
  px <- integer
  char ','
  many space
  py <- integer
  string "> velocity=<"
  many space
  vx <- integer
  char ','
  many space
  vy <- integer
  char '>'
  return $ P { pos = (px, py),  vel = (vx,vy) } 

integer = positiveInteger <|> negativeInteger
positiveInteger = read <$> many1 digit
negativeInteger = read <$> ( (:) <$> char '-' <*> many1 digit)

type Pos    = (Int, Int)
type Vel    = (Int, Int)
data Point  =  P {pos :: Pos, vel :: Vel} deriving Show
data Answer =  Answer 
                { getPoints :: [Point]
                , time      :: Int
                } 


main :: IO ()
main = do
 contents <- parseFile "input_day10.txt"
 let Right points = traverse (parse parsePoint "") contents
 let answer = forward (seed (updatePos <$>) points) 1 maxBound
 draw <$> boundaries <*> (fromList . (pos <$>)) $ getPoints answer
 print $ time answer
--


forward :: [[Point]] -> Int -> Int -> Answer
forward (previous:points:morePoints) time oldPerimeter
  | newPerimeter > oldPerimeter =
       Answer previous time
  | otherwise =
       forward (points:morePoints) (time+1) newPerimeter
  where
   (minX, maxX, minY, maxY) = boundaries points
   newPerimeter = 2 * (maxX - minX + maxY - minY)
  

boundaries :: [Point] -> (Int, Int, Int, Int)
boundaries points = 
  (minX, maxX, minY, maxY)
  where
   positions    = pos <$> points
   x_positions  = fst <$> positions
   y_positions  = snd <$> positions
   minX         = minimum x_positions
   maxX         = maximum x_positions
   minY         = minimum y_positions
   maxY         = maximum y_positions      

 
updatePos :: Point -> Point
updatePos (P (px,py) (vx, vy)) =
   P (px + vx, py + vy) (vx, vy)

seed :: (a -> a) -> a -> [a]
seed f x = let x' = f x in x': seed f x'


draw :: (Int, Int, Int, Int) -> Set Pos -> IO ()
draw (minX, maxX, minY, maxY) set =
 sequence_ [ drawCell row col maxX set 
            | row <- [minY..maxY]
            , col <- [minX..maxX]
           ]

drawCell row col maxX set = do
 if (col,row) `elem` set
   then putChar '#'
   else putChar ' '
 if col == maxX
   then putChar '\n'
   else return ()
