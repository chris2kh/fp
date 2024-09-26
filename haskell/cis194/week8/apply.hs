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

-- LESSON 2 https://fsharpforfunandprofit.com/posts/understanding-pars    er-combinators-2/
 
-- parse map
(<!>) :: (a -> b) -> Parser a -> Parser b
(<!>) f  parserA =
  let
   fun chars =
    case run parserA chars of
     Failure msg ->
      Failure msg
     Success x xs -> Success (f x) xs
   in Parser fun

-- Example
parseDigit = anyOf ['0'..'9']
parse3Digits' = parseDigit .>>. parseDigit .>>. parseDigit

flat3:: ((a,a), a) -> [a]
flat3 ((x1,x2), x3) = x1:x2:x3:[]
parse3Digits = flat3 <!> parse3Digits'

readInt :: String -> Integer
readInt = read
parse3DigitsAsInt = readInt <!> parse3Digits

-- So far we have create parsers that read chars
-- If we want to create parsers of strings we need
-- two new abstractions, return and apply


returnP :: a -> Parser a
returnP a =
 Parser (\s -> Success a s)

-- naive implementation
apply:: Parser ( a -> b) -> Parser a -> Parser b
apply f parserA =
  let 
   fun chars =
    case run parserA chars of
     Failure msg -> Failure msg
     Success x xs ->
      case run f xs of
       Failure msg -> Failure msg
       Success g gs -> Success (g x) gs
  in Parser fun 

-- elegant impl using abstractions
(<**>) :: Parser (a -> b) -> Parser a -> Parser b
(<**>) f parserA =
    (\ (f,x) -> f x) <!> (f .>>. parserA)

toInt :: Char -> String
toInt 'a' = "1"

parsePrueba :: Parser (Char -> String)
parsePrueba =
    let 
     fun chars =
       Success toInt chars
    in Parser fun

composicionApply :: 
    Parser (a -> b) -> 
    Parser a -> 
    Parser b
composicionApply pf p1 = pf <**> p1

composicionMap ::
    (a -> b) ->
    Parser a ->
    Parser b
composicionMap f p1 = f <!> p1


maxChar :: Char -> Char -> Integer
maxChar c1 c2
 | c1 > c2 = 1
 | otherwise = 2

lift2 :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
lift2 f pa pb =
    returnP f <**> pa <**> pb


(<|#|>) = lift2 (:)

