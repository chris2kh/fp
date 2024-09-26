module Main where

import qualified Data.Map.Strict as M

type GridSerial = Int
type SummedTableData = M.Map (Int, Int) Int

data SubGridData = SubGridData
                { left  :: Int
                , top   :: Int
                , total :: Int
                , size  :: Int
                } deriving Show

instance Eq SubGridData where
 sub1 == sub2 = total sub1 == total sub2

instance Ord SubGridData where
 sub1 > sub2  = total sub1 >  total sub2
 sub1 <= sub2 = total sub1 <= total sub2


(|>) :: a -> (a -> b) -> b
x |> f = f x

hundreds :: Int -> Int
hundreds = (`div` 100) . (`mod` 1000)

powerLevel :: GridSerial -> Int -> Int -> Int
powerLevel gridSerial col row = 
  rackId
  |> (* (row+1) )
  |> (+ gridSerial)
  |> (* rackId)
  |> hundreds
  |> (+ (-5))
  where rackId = (col+1) + 10

summedTablePowerLevels :: GridSerial -> M.Map (Int,Int) Int
summedTablePowerLevels gridSerial  =
  foldl fill  M.empty [ (row,col) | row <- [0..299], col <- [0..299] ]
  where 
   fill map (row,col) = let
   --  I(row, col) = I(row - 1, col) + I(row, col -1) - I(row-1, col-1)
   --                       a              b                  c
    a       = M.findWithDefault 0 (row - 1, col) map
    b       = M.findWithDefault 0 (row, col - 1) map
    c       = M.findWithDefault 0 (row - 1, col -1) map
    current = powerLevel gridSerial row col
    in M.insert (row, col) (current + a + b - c) map

subGrids :: [Int] -> SummedTableData -> [SubGridData]
subGrids sizes _I = do
   offset <- sizes
   row <- [-1..299-offset]
   col <- [-1..299-offset]
   -- a-------b
   --  ........
   -- c.......d
   -- subgrid =  (d + a) - (b + c)
   let a = M.findWithDefault 0  (row, col) _I
   let b = M.findWithDefault 0 (row, col+offset) _I
   let c = M.findWithDefault 0 (row+offset, col) _I
   let d = M.findWithDefault 0 (row+offset, col+offset) _I
   return $ SubGridData 
             { left = (col+1) + 1 -- add an extra one because we use zero
             , top  = (row+1) + 1 -- indexes but the problem description
                                  -- uses indexes starting at 1
             , total   = d + a - b - c
             , size    = offset
             }

solution :: [Int] -> SubGridData
solution sizes =
  5791
  |> summedTablePowerLevels
  |> subGrids sizes
  |> maximum


main :: IO ()
main = do
 let sol1 = solution [3]
 let sol2 = solution [2..300]
 print sol1
 print sol2

