module Day7 where

import Text.ParserCombinators.Parsec
import Data.Set
   ( empty
   , insert
   , toList
   , difference
   , union
   , fromList
   )
import Data.List (sort)
import qualified Data.Map as M

type Dependency = (Char, Char)

parseDependency :: Parser Dependency
parseDependency = do
    string "Step "
    mustBeFinished <- anyChar
    string " must be finished before step "
    beforeICanBegin <- anyChar
    string " can begin."
    return (beforeICanBegin, mustBeFinished)

main :: IO ()
main = do
  contents <- parseFile "inputDay7.txt"
  --contents <- parseFile "test.txt"
  let Right tuples = traverse (parse parseDependency "") contents
  let from         = foldr (\ (x,y) set -> insert x set) empty tuples 
  let to           = foldr (\ (_,y) set -> insert y set) empty tuples 
  let labels       = toList $ from `union` to
  let dictionary   = M.fromList $ zip labels [0..]
  let dependencies  = 
       (\(x,y) -> (dictionary M.! x, dictionary M.! y)) <$> tuples
  let tasks         = (dictionary M.!) <$> labels
  let emptyMatrix   = makeEmptyMatrix tasks
  let matrix        = arrange dependencies emptyMatrix

  print $ solve matrix dictionary

solve :: [[String]] -> (M.Map Char Int) -> String
solve [] _ = []
solve matrix dictionary  = 
 let 
  ready              = findNextReady matrix
  reducedMatrix      = crossout ready matrix  
  label              = getLabel ready dictionary
  reducedDictionary  = deleteAndReindex label dictionary 
 
 in label: solve reducedMatrix reducedDictionary

crossout  :: Int ->  [[String]] -> [[String]]
crossout task matrix  = do
   row <- remove matrix task --remove row that represent that task
   return $ remove row task  --for each of the remaining rows, remove
                             --the column that represent the interdepency
                             --to the removed task  

deleteAndReindex :: Char -> M.Map Char Int -> M.Map Char Int
deleteAndReindex label dictionary =
      M.fromList $ zip (M.keys $ M.delete label dictionary) [0..]


findNextReady :: [[String]] -> Int
findNextReady matrix =
  go matrix [] 0
  where
   go [] readies _ =  head $ sort readies
   go (x:xs) readies index | isReady x = go xs (index:readies) (index+1)
                           | otherwise = go xs readies (index+1)

getLabel :: Int -> M.Map Char Int -> Char
getLabel task dictionary =
 head $ M.keys $ M.filter (== task) dictionary


isReady :: [String] -> Bool
isReady = all (== "_") -- symbol to represent that a cell (x,y)
                       -- x doesnt depend on y to start

makeEmptyMatrix :: [Int] -> [[String]]
makeEmptyMatrix tasks = do
     tasks
     return $ take n $ repeat "_"
     where n = length tasks

arrange :: [(Int,Int)] -> [[String]] -> [[String]]
arrange [] matrix = matrix
arrange ((i, j):xs) matrix =
 arrange xs matrix'
  -- symbol to represent task x depends on y
  -- doesnt matter which symbols it is as long as it is not
  -- the symbol to represent no dependency 
  where row     = update "*" (matrix !! i) j 
        matrix' = update row matrix i

update :: a -> [a] -> Int -> [a]
update _ [] _ = error "cannot update empty list"
update val xs i =
 let (part1, (_:part2)) = splitAt i xs
 in
   part1 ++ (val:part2)

remove :: [a] -> Int -> [a]
remove [] _ = error "cannot remove from empty list"
remove x i =
  let (part1, (_:part2)) = splitAt i x
  in 
    part1 ++ part2 

parseFile :: String -> IO [String]
parseFile filename =
    readFile filename >>= (return . lines)

