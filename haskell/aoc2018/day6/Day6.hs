module Day6 where

import Data.List.Split
import Control.Monad.State
import qualified Data.Set as S
import qualified Data.Map as M
import Data.List (sort, maximumBy)
import Data.Ord (comparing)

parse :: String -> IO [String]
parse filename =
        readFile filename >>= (return . lines)

data Boundaries = Boundaries 
         { xmin :: Int
         , xmax :: Int
         , ymin :: Int
         , ymax :: Int
         } deriving Show

initialState = Boundaries maxBound minBound maxBound minBound


type Point = (Int, Int)
x = fst
y = snd

type Tag = String

data TaggedPoint = Tagged
        { tag   :: Tag
        , point :: (Int, Int)
        } deriving Show

type Distance = Int

main :: IO ()
main = do
  contents <- parse "inputDay6.txt"
  --contents <- parse "test.txt"
  let points =   (\[x,y] -> (read x, read y)) 
               . splitOn "," <$> contents :: [Point]

  -- tag points with 0,1,2,3...
  let taggedPoints = evalState (putTag points) (length points)
  
  let boundaries   = execState (findBoundaries points) initialState
  let grid         = makeGrid boundaries 
  let gridBorder   = makeGridBorder boundaries
 
  -- exclude these tags because they are points that extend to infinity
  -- also exclude "."
  let excludedTags = S.fromList 
                     $ "." : closests taggedPoints gridBorder

  let closestTags  = closests taggedPoints grid
  
  --let counts       = maximumBy (comparing snd)
  --                   $ M.toList
  --                   $ execState
  --                   (count closestTags excludedTags) M.empty
  --print counts
  let res2        = length 
                    $ filter ( (<10000) . sumManhattan taggedPoints) grid
  print res2


sumManhattan :: [TaggedPoint] -> Point -> Distance
sumManhattan taggedPoints p =
   foldr (\tp acc ->
           acc + (manhattanDistance p $ point tp)) 0 taggedPoints


makeGrid :: Boundaries -> [Point]
makeGrid boundaries = do
     x <- [xmin boundaries..xmax boundaries]
     y <- [ymin boundaries..ymax boundaries]
     return (x,y)     

makeGridBorder :: Boundaries -> [Point]
makeGridBorder Boundaries {xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax} =
  concat 
  [ [ (xmin,y) | y <- [ymin..ymax]]
  , [ (x,ymax) | x <- [xmin..xmax]]
  , [ (xmax,y) | y <- [ymin..ymax]]
  , [ (x,ymin) | x <- [xmin..xmax]]
  ]      
 
findBoundaries :: [Point] -> State Boundaries ()
findBoundaries [] = return ()
findBoundaries (p:ps) = do
              state <- get
              put $ Boundaries 
                    { xmin = min (xmin state) (x p)
                    , xmax = max (xmax state) (x p)
                    , ymin = min (ymin state) (y p)
                    , ymax = max (ymax state) (y p)
                    }
              findBoundaries ps 

fresh :: State Int Tag
fresh = do
    n <- get
    put (n-1)
    return $ show n

putTag :: [Point] -> State Int [TaggedPoint]
putTag [] = return []
putTag (p:ps) = do
     putTaggedTail <- putTag ps
     n <- fresh
     return $ (Tagged n p): putTaggedTail


closests :: [TaggedPoint] -> [Point] -> [Tag]
closests taggedPoints grid = do
  point <- grid
  return $ closest point taggedPoints 

closest :: Point -> [TaggedPoint] -> Tag
closest (x,y) taggedPoints =
  fst $ foldr (minTo x y) ("", maxBound) taggedPoints 
  where
  minTo x y tagged (closest, distance)
    | manhattan < distance  = (tag tagged, manhattan)
    | manhattan == distance = (".", manhattan)
    | otherwise = (closest, distance)
    where manhattan = manhattanDistance (x,y) (point tagged)

manhattanDistance :: Point -> Point -> Distance
manhattanDistance p1 p2 = abs ( x p2 - x p1) + abs (y p2 - y p1)

count :: [Tag] -> (S.Set Tag) -> State (M.Map Tag Int) ()
count [] _  = return ()
count (tag:tags) excluded = do
      dictionary <- get
      if tag `S.member` excluded
       then
         return ()
      else
         put (M.insertWith (+) tag 1 dictionary)
      count tags excluded
