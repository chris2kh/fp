module Main where
import qualified Data.Map as M
import Data.Char ( digitToInt
                 , intToDigit
                 )
import Data.List (isSuffixOf)
import qualified Data.Text as T


type Pos    = Int
type Value  = Int
type Scores = M.Map Pos Value

recipe :: Scores -> Pos -> Pos -> T.Text -> T.Text -> Int -> IO Int
recipe scores pos1 pos2 sequence original nums = do
        -- print just to see progress and not to stare
        -- at a blank screen while executing 
        if nums `mod` 10000 == 0 
          then print nums
          else return ()

        if original `T.isPrefixOf` sequence
         -- || original `T.isSuffixOf` (T.dropEnd 1 sequence)
          then return nums
          else do            
            let val1              = find pos1 scores
            let val2              = find pos2 scores
            let sum               = val1 + val2
            let scoresThisRound   = digits sum
            let oe                = intToDigit <$> scoresThisRound
            let newScores         = insert scoresThisRound scores
            let newLen            = len newScores
            let newPos1           = (pos1 + val1 + 1) `mod` newLen
            let newPos2           = (pos2 + val2 + 1) `mod` newLen
            let newNums           = nums + length scoresThisRound
            let newSequence       = advance sequence oe
              --print (val1, val2)
              --print scoresThisRound
            --print (newSequence)
              --print "__________________________"
            recipe newScores newPos1 newPos2 newSequence original newNums
                              

find :: Pos -> Scores -> Value
find = M.findWithDefault (-1)

advance :: T.Text -> String -> T.Text
advance xs [y] = T.tail xs `T.append` (T.pack [y])
advance xs [y1,y2] = T.tail xs `T.append` (T.pack $ show y1 ++ show y2)


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
 let target = T.pack "0124"
 let initialScores = (M.fromList [(0,3), (1,7)]) 
 answer <- recipe initialScores 0 1 (T.pack "37")  target 2
 print answer
