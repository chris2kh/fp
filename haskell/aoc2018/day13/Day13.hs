module Day13 where

import qualified Data.Map as M
import qualified Data.Set as S 
import Control.Monad.State

data Direction = 
   Up
 | Down
 | Left1
 | Right1
  deriving (Show, Eq)

data Path =
   Vertical
 | Horizontal
 | Intersection
 | Slash
 | BackSlash
  deriving (Show, Eq)

data Choice =
    Straight
  | ToLeft
  | ToRight
  deriving (Show, Eq)



type Track = M.Map (Int, Int) Path

data Car = Car
      { pos :: (Int, Int)
      , direction :: Direction
      , choices :: [Choice]
      } deriving Show

parseFile :: String -> IO [String]
parseFile filename = 
    readFile filename >>= (return . lines)

main :: IO ()
main = do
 contents <- parseFile "input_day13.txt"
 let (row,col,track, cars) = execState (loopRows contents) (0,0,M.empty,[])
 answer <- tick track cars 0
 print answer
 --print (row,col)


tick :: Track -> [Car] -> Int -> IO (Int, Int)
tick track cars round = do
   print round
   let cars1 = update cars track
   let oldPos   = pos <$> cars
   let cars' = filter (\car -> not $ (pos car) `elem` oldPos) cars1
   if (length cars') == 1 
        then return (pos $ head cars')
        else 
          case (isCrash cars') of
           []      -> tick track cars' (round + 1)
           crashes  ->
            do
            let cars'' = filter (\car -> not $ (pos car) `elem` crashes) cars'
            if (length cars'' == 1) then return (pos $ head $ cars'')
                                    else tick track cars'' (round+1)

isCrash :: [Car] -> [(Int, Int)]
isCrash cars = 
   fmap fst
 $ filter (\ (k,v) -> v > 1)
 $ M.toList
 $ foldr lambda M.empty cars
 where 
  lambda = \car dict -> M.insertWith (+) (pos car) 1 dict
        



loopRows :: [String] -> State (Int, Int, Track, [Car]) ()
loopRows [] = return ()
loopRows (row:rows) = do
   loopCols row 
   loopRows rows

loopCols :: String -> State (Int, Int, Track, [Car]) ()
loopCols [] = nextRow
loopCols (col:cols) = do
   inspect col
   loopCols cols


nextRow :: State (Int, Int, Track, [Car]) ()
nextRow = modify $ \(row, _, track, cars) -> (row+1, 0, track, cars)

nextCol :: State (Int, Int, Track, [Car]) ()
nextCol = modify $ \(row, col, track, cars) -> (row, col+1, track, cars)


inspect :: Char -> State (Int, Int, Track, [Car]) ()
inspect ' ' = nextCol
inspect char 
  | char `elem` ['^', '<', 'v', '>'] = do
    (row, col, track, cars) <- get
    let car = Car (row,col) (toDirection char) (cycle [ToLeft,Straight,ToRight])
    let track' = M.insert (row,col) (toPath char) track
    put (row,col, track', (car:cars))
    nextCol
  | otherwise = do 
    (row, col, track, cars) <- get
    let track' = M.insert (row, col) (toPath char) track
    put (row,col, track', cars)
    nextCol

toDirection :: Char -> Direction
toDirection char = 
    case char of
     '^' -> Up
     'v' -> Down
     '<' -> Left1
     '>' -> Right1

toPath :: Char -> Path
toPath char =
    case char of
      '|'  -> Vertical
      '-'  -> Horizontal
      '+'  -> Intersection
      '/'  -> Slash
      '\\' -> BackSlash
      '^'  -> Vertical
      'v'  -> Vertical
      '<'  -> Horizontal
      '>'  -> Horizontal

update :: [Car] -> Track -> [Car]
update cars track = helper <$> cars
 where 
  helper car = let
   pos       = nextPosition car
   dir       = nextDirection (track M.! pos) car
   nextInter = if ((track M.! pos) == Intersection) 
                then tail $ choices car
                else choices car
   in
    Car pos dir nextInter

nextPosition :: Car -> (Int, Int) 
nextPosition (Car {direction = direction, pos = (row, col)}) = 
   case direction of
    Up     -> (row - 1, col)
    Down   -> (row + 1, col)
    Left1  -> (row, col - 1)
    Right1 -> (row, col + 1)
    
nextDirection :: Path -> Car -> Direction
nextDirection path car = case (path, direction car) of
   (Vertical,   Up)             -> Up
   (Vertical,   Down)           -> Down
   (Horizontal, Left1)          -> Left1
   (Horizontal, Right1)         -> Right1
   (Slash, Up)                  -> Right1
   (Slash, Down)                -> Left1
   (Slash, Left1)               -> Down
   (Slash, Right1)              -> Up
   (BackSlash, Up)              -> Left1
   (BackSlash, Down)            -> Right1
   (BackSlash, Left1)           -> Up
   (BackSlash, Right1)          -> Down
   (Intersection, direction)    -> decideIntersection ( head $ choices car) direction
   (path, direction)    -> error $ "path: " ++ (show path) 
                              ++ ", direction: " ++ (show direction)
                              ++ " not possible"
                                  

decideIntersection :: Choice -> Direction -> Direction
decideIntersection choice dir = case (choice, dir) of
   (Straight, Up)    -> Up
   (ToLeft, Up)      -> Left1
   (ToRight, Up)     -> Right1 
   (Straight, Down)  -> Down
   (ToLeft, Down)    -> Right1
   (ToRight, Down)   -> Left1 
   (Straight, Left1) -> Left1
   (ToLeft, Left1)    -> Down
   (ToRight, Left1)   -> Up
   (Straight, Right1) -> Right1
   (ToLeft, Right1)    -> Up
   (ToRight, Right1)   -> Down

{-
data Car = Car
      { pos :: (Int, Int)
      , direction :: Direction
      , choices :: [Direction]
      } deriving Show
-}

