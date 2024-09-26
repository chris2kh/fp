module Main where
import qualified Data.Map as M
import Data.Char ( digitToInt
                 , intToDigit
                 )

type Pos    = Int
type Value  = Int
type Scores = M.Map Pos Value

recipe :: Scores -> Pos -> Pos -> Value -> IO Scores
recipe scores pos1 pos2 targetValue = do
        -- print just to see progress and not to stare
        -- at a blank screen while executing 
        let n = len scores
        if n `mod` 10000 == 0 
          then print n
          else return ()
          
        if n >= targetValue
          then return scores
          else let            
            val1      = find pos1 scores
            val2      = find pos2 scores
            sum       = val1 + val2
            newScores = insert (digits sum) scores
            newLen    = len newScores
            newPos1   = (pos1 + val1 + 1) `mod` newLen
            newPos2   = (pos2 + val2 + 1) `mod` newLen
            in recipe newScores newPos1 newPos2 targetValue

find :: Pos -> Scores -> Value
find = M.findWithDefault (-1)

len :: Scores -> Int
len = M.size

toTuple :: Scores -> [(Pos,Value)]
toTuple = M.toList


insert :: [Value] -> Scores -> Scores
insert  values scores =
  foldl f scores (zip [len scores ..] values)
   where
     f = flip $ uncurry M.insert
 
digits :: Value -> [Value]
digits = map digitToInt . show

toString :: [Int] -> String
toString = show . map intToDigit

between :: Ord a => a -> (a,a) -> Bool
between x (low, high) = x >= low && x < high 

main :: IO()
main = do
 let start = 21000000
 let end = start + 10
 let initialScores = (M.fromList [(0,3), (1,7)]) 

 finalScores <- recipe initialScores 0 1 end
 let answer = toString
            $ fmap snd   
            $ filter ((`between` (start,end)) . fst)
            $ toTuple finalScores
 putStrLn answer
