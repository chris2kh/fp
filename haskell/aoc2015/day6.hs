{-# LANGUAGE RecordWildCards #-}

module Day6 where
import Utils
import Text.ParserCombinators.Parsec
import qualified Data.Map.Strict as M
import Data.List

data Command = ON | OFF | TOGGLE deriving Show
type Lights = M.Map (Int, Int) Int

data Instruction =
     Instruction 
     { command :: Command
     , from    :: (Int, Int)
     , to      :: (Int, Int)
     } deriving Show

integer = read <$> many1 digit

parseON     = ON     <$ string "turn on "
parseOFF    = OFF    <$ string "turn off "
parseTOGGLE = TOGGLE <$ string "toggle "

instruction :: Parser Instruction
instruction = do
    command <- try parseON <|> try parseOFF <|> parseTOGGLE
    fromX <- integer
    char ','
    fromY <- integer
    string " through "
    toX <- integer
    char ','
    toY <- integer

    return $ Instruction command (fromX, fromY) (toX, toY) 

initialState :: Lights
initialState = M.empty


main :: IO ()
main = do
  input <- parseList "inputs/day6.txt"
  let Right instructions = traverse (parse instruction "") input
  let finalState = foldl' (apply rules2) initialState instructions
  print $ sum $ getResults finalState 

apply :: (Command -> Lights -> (Int, Int) -> Lights)
      -> Lights
      -> Instruction
      -> Lights

apply modify lights Instruction {..}  =
  foldl' 
   (modify command) 
   lights 
   [ (x,y) 
   |  x <- [fst from .. fst to]
   ,  y <- [snd from .. snd to]
   ]
       
rules1, rules2 :: Command -> Lights -> (Int, Int) -> Lights

rules1 ON     lights pos = M.insert pos 1  lights
rules1 OFF    lights pos = M.insert pos 0 lights
rules1 TOGGLE lights pos = case (M.findWithDefault 0 pos lights) of
                             1 -> M.insert pos 0 lights
                             _ -> M.insert pos 1  lights

rules2 ON     lights pos = M.insertWith (+)                    pos 1 lights
rules2 OFF    lights pos = M.insertWith (flip (max . ((-) 1))) pos 0 lights
rules2 TOGGLE lights pos = M.insertWith (+)                    pos 2 lights

getResults :: Lights -> [Int]
getResults lights = snd <$> M.toList lights
