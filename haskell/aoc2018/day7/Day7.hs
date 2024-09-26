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

type Dependency = (Int, Int)


data Worker = 
   Free
 | W { task :: Task, remaining :: Int }
  deriving (Show, Eq)


type Matrix = [[String]]
type Row    = [String]
type Col    = Int

type Clock = Int
type Step  = Char
type Task  = Int -- A = 0, B = 1, C = 2....

parseTuple :: Parser (Char, Char)
parseTuple = do
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
  let Right tuples = traverse (parse parseTuple "") contents
  let from         = foldr (\ (x,_) set -> insert x set) empty tuples 
  let to           = foldr (\ (_,y) set -> insert y set) empty tuples 
  let steps        = toList $ from `union` to
  let dictionary   = M.fromList $ zip [0..] steps
  let dependencies = 
       (\(x,y) -> (,) <$> findKey x <*> findKey y $ dictionary) <$> tuples
  let emptyMatrix  = makeEmptyMatrix (length steps)
  let matrix       = arrange dependencies emptyMatrix

  (_, res1) <- solve matrix (take 1 $ repeat Free) dictionary 0 []
  (res2, _) <- solve matrix (take 5 $ repeat Free) dictionary 0 []
  print res1
  print res2

findKey :: Eq v => v -> M.Map k v -> k
findKey v dict =  head $ M.keys $ M.filter (== v) dict

makeEmptyMatrix :: Int -> Matrix
makeEmptyMatrix n =
     [ take n $ repeat "_" | _ <- [0..(n-1)]]

arrange :: [Dependency] -> Matrix -> Matrix
arrange [] matrix = matrix
arrange ((i, j):xs) matrix =
 arrange xs matrix'
  -- symbol to represent task x depends on y
  -- doesnt matter which symbols it is as long as it is not
  -- the symbol to represent no dependency 
  where row     = update "*" (matrix !! i) j 
        matrix' = update row matrix i


solve :: Matrix -> 
         [Worker] -> M.Map Task Step -> Clock -> [Step] ->  IO (Clock, [Step])
solve matrix workers dictionary clock steps = do
  --putStrLn "starting iteration.."
  --putStrLn (show clock ++ "  " ++ show workers)
  let tasksCompleted =  task <$> filter justFinished workers

  let steps' = case tasksCompleted of
              [] -> steps
              (task:_) -> do (dictionary M.! task):steps
  
  let workers' = markFree <$> workers
  --putStrLn $ "completed tasks : " ++ show tasksCompleted 
  let matrix' = markCompleted tasksCompleted matrix 
  --putStrLn "showing matrix with completed tasks on board :" 
  --print matrix'
  let canBegin = findCanBegin matrix'
  --putStrLn "found the following tasks that can begin:"
  --print $ canBegin

  -- finished!!
  if all (== Free) workers' && canBegin == [] 
    then do
      -- putStrLn "entered stop condition"
      -- putStrLn $ "seconds ellapsed: " ++ show clock
      -- putStrLn $ "workers state:  " ++ show workers
      return (clock, reverse steps')
  else do
    let (assignedTasks, workers'') = tryAssign canBegin workers'
    -- putStrLn "after trying to assign tasks to workers: "
    -- putStrLn $ "assigned Tasks: " ++ show assignedTasks
    -- putStrLn $ "workers :  " ++ show workers'

    let matrix'' = markStarted assignedTasks matrix' 
    --putStrLn "showing matirx with recently assigned/started tasks on board :"
    --print matrix''
    let (workers''', clock') = tick workers'' clock
    --putStrLn "tick clock..."
    --putStrLn ""
    --putStrLn ""
    solve matrix'' workers''' dictionary clock' steps'


justFinished :: Worker -> Bool
justFinished W {remaining = 0} = True
justFinished _ = False

tick :: [Worker] -> Clock -> ([Worker], Clock)
tick workers clock = 
  (doWork <$> workers, clock + 1)

doWork :: Worker -> Worker
doWork w@(W {remaining = n}) | n > 0 = w { remaining = remaining w -1 }
doWork w = w


markFree :: Worker -> Worker
markFree w | justFinished w = Free
           | otherwise = w 

tryAssign :: [Task] -> [Worker] -> ([Task], [Worker])
tryAssign tasks workers =
 go tasks workers ([],[])
 where
  go [] ws (ats, ws') = (ats, reverse ws' ++ ws)
  go _ [] (ats, ws') = (ats, reverse ws')
  go tasks@(t:ts) (w:ws) (ats, ws') = 
    case w of
     Free ->  go ts ws (t:ats, (W t (t+61)):ws')
     _    ->  go tasks ws (ats, w:ws')


markStarted :: [Task] -> Matrix -> Matrix
markStarted started matrix =
  go started matrix
  where
   go [] matrix = matrix   
   go (row:rows) matrix  =
     go rows (update (repeat "started") matrix row)

markCompleted :: [Task] -> Matrix -> Matrix
markCompleted completed matrix = do
  row <- matrix
  -- for each row update all columns corresponding to completed tasks
  return $ foldl (update "_") row completed
   
-- find all rows that don't have an active dependecy an are available to begin
findCanBegin :: Matrix -> [Task]
findCanBegin matrix =
  go [] 0
  where
   go candidates task 
       | task == length matrix = sort candidates
       | canBegin task matrix  = go (task:candidates) (task+1)
       | otherwise = go candidates (task+1)

canBegin :: Task -> Matrix  -> Bool
-- symbol to represent that a in a cell (x,y) x does not depend on y to begin
canBegin task matrix = all (== "_") $ matrix !! task

update :: a -> [a] -> Int -> [a]
update _ [] _ = error "cannot update empty list"
update val xs i =
 let (part1, (_:part2)) = splitAt i xs
 in
   part1 ++ (val:part2)

parseFile :: String -> IO [String]
parseFile filename =
    readFile filename >>= (return . lines)
