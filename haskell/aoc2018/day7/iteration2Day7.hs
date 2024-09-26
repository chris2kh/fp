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
  let from         = foldr (\ (x,_) set -> insert x set) empty tuples 
  let to           = foldr (\ (_,y) set -> insert y set) empty tuples 
  let steps        = toList $ from `union` to
  let dictionary   = M.fromList $ zip [0..] steps
  let dependencyIndexes = 
       (\(x,y) -> (,) <$> findKey x <*> findKey y $ dictionary) <$> tuples
  let stepIndexes  = M.keys dictionary
  let emptyMatrix  = makeEmptyMatrix stepIndexes
  let matrix       = arrange dependencyIndexes emptyMatrix

  --print $ solve1 matrix dictionary
  res <- solve2 matrix (take 1 $ repeat ((-1),0)) 0 dictionary
  print res



solve2 :: [[String]] -> [(Int,Int)] -> Int -> M.Map Int Char -> IO Int
solve2 matrix workers clock dictionary = do
  --putStrLn "entrando"
  --putStrLn (show clock ++ "  " ++ show workers)
  let completed =  fst <$> 
          filter (\w -> fst w /= (-1) && snd w == 0) workers
  --putStrLn $ "tareas completadas : " ++ show completed 
  case completed of
    [] -> return ()
    (x:_) -> do
        print (dictionary M.! x)

  let matrix' = markCompleted completed matrix 
  --putStrLn "matrix con tareas completas marcadas:" 
  --print matrix'
  let nextReadies = findNextReadies matrix'
  --putStrLn "los siguientes readies son:"
  --print $ nextReadies  

  if all (== 0) (snd <$> workers) && nextReadies == [] 
    then do
      --putStrLn "entrando a la condicion de parada"
      --putStrLn $ "el reloj va en el segundo: " ++ show clock
      --putStrLn $ "y el estado de los workers es:  " ++ show workers
      return clock
  else do
    let (assignedTasks, workers') = tryAssign nextReadies workers
    --putStrLn "luego de tratar de asignar los next readies tenemos: "
    --putStrLn $ "assigned Tasks: " ++ show assignedTasks
    --putStrLn $ "workers :  " ++ show workers'

    let matrix''   = markStarted assignedTasks matrix' 
    --putStrLn "luego de marcar en la matrix las asignadas, la matrix es :"
    --print matrix''
    let (workers'', clock') = tick workers' clock
    --putStrLn "haciendo tick"
    --putStrLn ""
    --putStrLn ""
    solve2 matrix'' workers'' clock' dictionary
    --return 0


tick :: [(Int,Int)] -> Int -> ([(Int, Int)], Int)
tick workers clock =
   (w, clock + 1)
   where
    w =  (\w -> if snd w == 0 then w else (fst w, snd w -1)) <$> workers 


tryAssign :: [Int] -> [(Int,Int)] -> ([Int], [(Int, Int)])
tryAssign tasks workers = 
 go tasks workers ([],[])
 where
  go [] ws (ats, ws') = (ats, reverse ws' ++ ws)
  go _ [] (ats, ws') = (ats, reverse ws')
  go t_@(t:ts) (w:ws) (ats, ws')
    -- assign a task only if worker previous task duration is now 0
   | snd w == 0  = go ts ws (t:ats, (t,t+61):ws')
   | otherwise  = go t_ ws (ats, w:ws') 

markStarted :: [Int] -> [[String]] -> [[String]]
markStarted assigned matrix =
  go assigned matrix (length matrix)
  where
   go [] matrix _ = matrix   
   go (a:as) matrix n =
     go as (update (take n $ repeat "s") matrix a) n

markCompleted :: [Int] -> [[String]] -> [[String]]
markCompleted completed matrix = do
  row <- matrix
  return $ something row
  where
   something row =
     foldr (\index row ->
              if row !! index == "s" then row else update "_" row index) row completed
   

findKey :: Eq v => v -> M.Map k v -> k
findKey v dict =  head $ M.keys $ M.filter (== v) dict



solve1 :: [[String]] -> (M.Map Int Char) -> String
solve1 matrix dictionary  = 
 let 
  nextReadies    = findNextReadies matrix
  readyIndex:_   = nextReadies
  updatedMatrix  = crossout readyIndex matrix  
  step           = dictionary M.! readyIndex
 
 in case nextReadies of
     [] -> []
     _  -> step: solve1 updatedMatrix dictionary

crossout  :: Int ->  [[String]] -> [[String]]
crossout index matrix  = do
   row <- update (repeat "done") matrix index
   return $ update "_" row index
                            
findNextReadies :: [[String]] -> [Int]
findNextReadies matrix =
  go matrix [] 0
  where
   go [] readies _ = sort readies
   go (x:xs) readies index | isReady x = go xs (index:readies) (index+1)
                           | otherwise = go xs readies (index+1)

isReady :: [String] -> Bool
isReady = all (== "_") -- symbol to represent that a cell (x,y)
                       -- x doesnt depend on y to start

makeEmptyMatrix :: [Int] -> [[String]]
makeEmptyMatrix indexes = do
     indexes
     return $ take n $ repeat "_"
     where n = length indexes

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

parseFile :: String -> IO [String]
parseFile filename =
    readFile filename >>= (return . lines)

