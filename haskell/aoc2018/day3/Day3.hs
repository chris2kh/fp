module Day3 where

import Text.ParserCombinators.Parsec hiding (count)
import qualified Control.Monad.State as S
import Data.List (sort, find)
import qualified Data.Map as M
   ( empty
   , insertWith
   , Map
   , elems
   , filter
   , keys
   , (!)
   )

data Claim = 
 Claim 
  { id           :: Int
  , paddingLeft  :: Int
  , paddingTop   :: Int
  , width        :: Int
  , height       :: Int
  } deriving Show

integer = read <$> many1 digit

claim :: Parser Claim
claim = do
     char '#'
     id <- integer
     string " @ "
     pl <- integer
     char ','
     pt <- integer
     string ": "
     w <- integer
     char 'x'
     h <- integer
     return $ Claim id pl pt w h

main :: IO ()
main = do
  input <- parseFile "inputDay3.txt"
  let Right claims = traverse (parse claim "") input
  let count =  S.execState (countCellsFreq claims) M.empty
  
  putStrLn $ "the answer for problem 1 is: " ++
     ( show
     . length
     . filter (>1)
     . M.elems
     ) count

  -- count is a map 
  -- count M.! cell is get me the value whose entry in the map is cell
  let Just found = find (\claim ->
                    all (\cell -> count M.! cell == 1) (cells claim)) 
                   claims 

  putStrLn $ "the answer for problem 2 is: " ++ (show . Day3.id) found
 
parseFile :: String -> IO [String] 
parseFile filename = readFile filename >>= ( return . lines)

countCellsFreq :: [Claim] -> S.State (M.Map (Int, Int) Int) ()
countCellsFreq [] = return ()
countCellsFreq (claim:claims) = do
  S.modify (\map -> foldr updateCellCount map $ cells claim)
  countCellsFreq claims

updateCellCount :: (Int, Int) -> M.Map (Int, Int) Int -> M.Map (Int, Int) Int 
-- if entry doesnt exist, first visit = 1 -- otherwise add 1
updateCellCount cell map = M.insertWith (+) cell 1 map

cells :: Claim -> [(Int, Int)]
cells claim =
 [ (x,y) | x <- [x1..x2], y <- [y1..y2]]
  where x1 = paddingLeft claim   + 1
        x2 = x1 + width claim    - 1
        y1 = paddingTop claim    + 1
        y2 = y1 + height claim   - 1  
