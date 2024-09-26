module Day7 where

import Utils
import Text.ParserCombinators.Parsec hiding (State)
import Data.Char
import Data.Bits
import qualified Data.Map as M
import Control.Monad.State

parseDatum :: Parser Datum
parseDatum = do
  datum <- try (many1 digit) <|> many1 letter
  if isDigit $ head datum
  then return $ Num (read datum)
  else return $ Var datum

parseAnd, parseOr, parseLshift, parseRshift, parseNot :: Parser Expression

parseAnd = do
  x <- parseDatum
  space
  string "AND"
  space
  y <- parseDatum
  space
  return $ And x y

parseOr = do
  x <- parseDatum
  space
  string "OR"
  space
  y <- parseDatum
  space
  return $ Or x y

parseLshift = do
  x <- parseDatum
  space
  string "LSHIFT"
  space
  y <- parseDatum
  space
  return $ Lshift x y

parseRshift = do
  x <- parseDatum
  space
  string "RSHIFT"
  space
  y <- parseDatum
  space
  return $ Rshift x y

parseNot = do
  string "NOT"
  space
  x <- parseDatum
  space
  return $ Not x

parseSingle = do
  x <- parseDatum
  space
  return $ Single x 

parseExpression :: Parser Expression
parseExpression = try parseAnd 
              <|> try parseOr 
              <|> try parseLshift
              <|> try parseRshift
              <|> try parseNot
              <|> parseSingle


parseRow :: Parser (String, Expression)
parseRow = do
  expression <- parseExpression
  string "->"
  space
  key <- many1 letter 
  return (key, expression)

type Environment = M.Map String Expression

freshEnvironment :: Environment
freshEnvironment = M.empty

add :: (String, Expression) -> Environment -> Environment
add (key, op) env = update key op env

find :: String -> Environment -> Expression
find x env = env M.! x

update :: String -> Expression -> Environment -> Environment
update key newval env = M.insert key newval env

main :: IO ()
main = do
 input <- parseList "inputs/day7.txt"
 let Right records = traverse (parse parseRow "") input
 let environment   = foldr add freshEnvironment records 
 
 let res1          = evalState (solveFor "a") environment
 print res1
 
 let environment'  = update "b" (Single $ Num res1) environment
 let res2          = evalState (solveFor "a") environment'
 print res2

eval :: Datum -> State Environment Int
eval (Var x) = do
           answer <- solveFor x
           -- memoize results because gates can appear many times
           -- so you dont have to compute all over again
           modify (\ s -> update x (Single $ Num answer) s)
           return answer
            
eval (Num x) = return x

solveFor :: String -> State Environment Int
solveFor key = do
 env <- get
 case (find key env) of
  Single d -> eval d
  And x y    -> do 
                x' <- eval x
                y' <- eval y
                return $ x'.&. y' 
                
  Or x y     -> do 
                x' <- eval x
                y' <- eval y
                return $ x'.|. y' 

  Lshift x y -> do
                x' <- eval x 
                y' <- eval y
                return $ x' `shiftL` y'

  Rshift x y -> do
                x' <- eval x 
                y' <- eval y
                return $ x' `shiftR` y'
 
  Not x      -> do
                x' <- eval x
                return $ complement x'

data Datum = Var String | Num Int
data Expression = Single Datum 
                | And    Datum Datum
                | Or     Datum Datum
                | Lshift Datum Datum
                | Rshift Datum Datum
                | Not    Datum
