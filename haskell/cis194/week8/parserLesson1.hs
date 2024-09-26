import Text.Printf

-- encapsulate result
data ParseResult a =
    Success a String
  | Failure String
   deriving Show

-- encapsulate function
data Parser a = Parser (String -> ParseResult a)

-- unwrap function and apply it
run:: Parser a -> String -> ParseResult a
run (Parser p) = p

-- building  block of parsers
-- partial apply to include desired char in parser
pchar :: Char -> Parser Char
pchar c =
    let
      fun [] = Failure "No more input"
      fun xs
       | head xs == c = 
          Success c (tail xs)
       | otherwise =
          Failure $ printf "Expecting %c Got %c" c (head xs)
    in Parser fun

-- a couple of useful parsers for testing
parseA = pchar 'A'
parseB = pchar 'B'
parseC = pchar 'C'
parseD = pchar 'D'

-- Examples
-- run parseA "ABC" == Success 'A' "BC"
-- run parseA "BC" == Failure "Expecting A Got B"



-- Lets start composing parsers!

-- andThen. Apply parseA then parseB
(.>>.) :: Parser a -> Parser b -> Parser (a, b)
(.>>.) parserA parserB =
 let 
  fun chars = 

   case run parserA chars of
    Failure f -> 
     Failure f
    
    Success x xs ->  
     case run parserB xs of
      Failure msg -> 
       Failure msg
      
      Success y ys -> 
       Success (x,y) ys
  
  in Parser fun

-- Examples
-- parseAthenB = parseA .>>. parseB
-- run parseAthenB "ABC" == Success ('A', 'B') "C"
-- run parseAthenB "ACC" == Failure "Expecting B Got C"
-- run (parseA .>>. (parseB .>>. parseC .>>. parseD)) "ABCDEFGH" ==
-- Success ('A',(('B','C'),'D')) "EFGH"

-- orElse
(<|>) :: Parser a -> Parser a -> Parser a
(<|>) parserA parserB =
 let 
  fun chars = 

   case run parserA chars of
    Success a s -> 
     Success a s
    
    Failure _ ->  
     case run parserB chars of
      Success a s -> 
       Success a s
      
      Failure msg -> 
       Failure msg
  
  in Parser fun 
  
 -- Examples
-- parseAAndThenBorC = parseA .>>. (parseB <|> parseC)
-- run parseAAndThenBorC "ABC" = Success ('A', 'B') "C"
-- run parseAAndThenBorC "ACC" = Success ('A', 'C') "C"
-- run parseAAndThenBorC "AD" = Failure "Expecting B Got D"


-- fold a list of parsers into one parser
-- using the orElse combinator
choice :: [Parser a] -> Parser a 
choice = foldl1 (<|>)

-- parse anyChar from a list of chars
anyOf :: [Char] -> Parser Char
anyOf = choice . (pchar <$>)

-- Examples
-- parseLowercase = anyOf ['a'..'z']
-- parseDigit = anyOf ['0'..'9']

--run parseLowercase "aBC" == Success 'a' "BC"
--run parseLowercase "ABC" == Failure "Expecting 'z'. Got 'A'"

--run parseDigit "1ABC" == Success '1' "ABC"
--run parseDigit "9ABC" == Success '9' "ABC"
--run parseDigit "|ABC" == Failure "Expecting '9'. Got '|'"

