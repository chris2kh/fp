{-# LANGUAGE NamedFieldPuns #-}
import Text.Printf
import qualified Data.Char as C
import qualified Data.Map as M

type ParserLabel = String
type ParserError = String

data Position = Position 
 { line :: Integer
 , col  :: Integer
 }

-- to display error msgs
data ParserPosition = ParserPosition
 { currentLine :: String
 , line1 :: Integer
 , col1  :: Integer
 }

fromInputState :: InputState -> ParserPosition
fromInputState input =
 ParserPosition { currentLine, line1, col1 }
  where
   currentLine = (rows $ input) !! fromInteger (line $ position input)
   line1 = toInteger (line $ position input)
   col1 =  toInteger (col  $ position input)

initPos = Position { line = 0, col= 0} 

incrLine :: Position -> Position
incrLine p = Position { line = (line p) + 1, col = 0}

incrCol :: Position -> Position
incrCol p = p { col = (col p) + 1}

data InputState = InputState
  { rows    :: [String]
  , position :: Position
  }

fromStr :: String -> InputState
fromStr xs =
  InputState {rows = lines xs, position = initPos}

nextChar :: InputState -> (InputState, Maybe Char)
nextChar input
   -- end of file
   | line' >= max_line =
       ( input
       , Nothing) 
   
   -- end of line
   | col' >= max_col =
       ( input { position = incrLine $ position input }
       , Just '\n')
   
   -- still in the same line
   | otherwise =
       ( input { position = incrCol $ position input }
       , Just token)
   where 
    col'     = col  $ position input
    line'    = line $ position input
    max_line = toInteger $ length $ rows input 
    max_col  = toInteger $ length $ rows input !! fromInteger line' 
    token    = rows input !! fromInteger line' !! fromInteger col' 

readAllChars :: InputState -> String
readAllChars input =
  case nextChar input of
   (_, Nothing) -> []
   (input', Just c) -> c:(readAllChars input')




-- encapsulate result
data ParseResult a =
    Success a InputState
  | Failure ParserLabel ParserError ParserPosition


-- encapsulate function
data Parser a = 
  Parser 
  { parseFn :: (InputState -> ParseResult a)
  , label :: ParserLabel
  }

setLabel :: Parser a -> ParserLabel -> Parser a
setLabel pa label=
    let
     fun chars =
      case runOnInput pa chars of
       Success x xs -> Success x xs
       Failure l e p -> Failure label e p
    in Parser {parseFn = fun, label = label}

(<?>) = setLabel

printResult :: Show a =>  ParseResult a -> String
printResult (Failure label error parsePos) =
  let
   errorLine = currentLine parsePos
   colPos = col1 parsePos
   linePos = line1 parsePos
   failureCaret = error
   --failureCaret = printf "%*s^%s" colPos "" error
  in
   printf "Line:%i Col:%i Error parsing %s /n %s /n %s" linePos colPos label errorLine failureCaret
printResult (Success x xs) =
 show x 

-- unwrap function and apply it
run:: Parser a -> String -> ParseResult a
run p = runOnInput p . fromStr


runOnInput :: Parser a -> InputState -> ParseResult a
runOnInput  (Parser {parseFn = p}) = p

-- building  block of parsers
-- partial apply to include desired char in parser
pchar :: Char -> Parser Char
pchar c =
    satisfy (\x -> x == c) [c]

satisfy :: (Char -> Bool) -> ParserLabel -> Parser Char
satisfy predicate label =
 let
   fun input =
    case nextChar input of
     (_ ,Nothing)->
       Failure
        label
        "No more input"
        (fromInputState input)
     
     (input', Just x)
       | predicate x -> 
           Success x input'
       | otherwise -> 
           Failure
            label
            (printf "Unexpected '%c'" x)
            (fromInputState input) 

   in Parser {parseFn = fun, label = label } 

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

   case runOnInput parserA chars of
    Failure l e p -> 
     Failure l e p
    
    Success x xs ->  
     case runOnInput parserB xs of
      Failure l e p -> 
       Failure l e p
      
      Success y ys -> 
       Success (x,y) ys
  
 in Parser 
   { parseFn = fun
   , label   = printf "%s andThen %s" 
               (label parserA) 
               (label parserB) 
   }

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

   case runOnInput parserA chars of
    Success a s -> 
     Success a s
    
    Failure _ _ _ ->  
     case runOnInput parserB chars of
      Success a s -> 
       Success a s
      
      Failure l e p -> 
       Failure l e p
  
 in Parser 
   { parseFn = fun
   , label   = printf "%s orElse %s" 
               (label parserA) 
               (label parserB) 
   }
  
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
    case runOnInput parserA chars of
     Failure l e p ->
      Failure  l e p
     Success x xs -> Success (f x) xs
  in Parser 
   { parseFn = fun
   , label   = "unknown" 
   }

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
 Parser { parseFn = (\s -> Success a s), label = "unknown"} 

-- naive implementation
apply:: Parser ( a -> b) -> Parser a -> Parser b
apply f parserA =
 let 
   fun chars =
    case runOnInput parserA chars of
     Failure  l e p -> Failure l e p
     Success x xs ->
      case runOnInput f xs of
       Failure  l e p -> Failure l e p
       Success g gs -> Success (g x) gs
  in Parser { parseFn = fun, label = "unknown" }  

-- elegant impl using abstractions
(<**>) :: Parser (a -> b) -> Parser a -> Parser b
(<**>) f parserA =
    (\ (f,x) -> f x) <!> (f .>>. parserA)

lift2 :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
lift2 f parserA parserB =
      returnP f <**> parserA <**> parserB

{--
para levantar funciones de un parametro, puedo usar tanto fmap
como apply

para un param
t: fmap   (a -> b) -> Parser a -> Parser b
t: apply P(a -> b) -> Parser a -> Parser b

La ventaja de apply es que si necesito levantar funciones de
varios parametros (util por ejemplo cuando quiero encadenar varios cahrs que conforman un string), lo puedo hacer gracias a que
encapsula dentro del Parser funciones parciales que recuerdan el param
anterior.  Con fmap no lo puedo realizar

:t lift2 (a -> b -> c) -> Parser a -> Parser b -> Parser c
lift2 f pa pb =
 ( returnP f <**> pa)  <**> pb
 ( esto me devuelve  
   un parser de
   (b -> c)

  Parser (b -> c)     <**>  pb  = Parser c

me conserva la estructura del mundo normal de funciones de 2 params 
en el mundo de los parser

Ej: :t (+)        Num a => a  -> a ->  a
    :t lift2 (+)  Num a => a  -> Pa -> Pa

--}

consP = lift2 (:)

sequenceP [] = returnP []
sequenceP (x:xs) =
     consP x (sequenceP xs)

pstring :: String -> Parser String
pstring = sequenceP . fmap pchar

-- examples
--run (pstring "ABC") "ABCFGFGFF"
--run (pstring "ABC") "ABCFGFGFF"
--run (pstring "ABC") "A|EDRFF"

many :: Parser a -> Parser [a]
many pa =
  let 
    -- fun acc [] = Success acc ""
    fun acc chars =
         case runOnInput pa chars of
           Failure _ _ _ -> Success acc chars
           Success x xs -> fun (acc ++ [x]) xs
  in 
   Parser 
   { parseFn = fun []
   , label = printf "many %s" (label pa)
   }

many1 :: Parser a -> Parser [a]
many1 pa =
  let
   fun chars =
    case runOnInput pa chars of
     Failure  l e p -> Failure  l e p
     Success x xs -> 
          Success (x:x') xs'
          where Success x' xs' = runOnInput (many pa) xs
  in
   Parser 
   { parseFn = fun
   , label = printf "many1 %s" (label pa)
   }

digit :: Parser Char
digit = anyOf ['0'..'9']
digits = many1 digit

-- run digits "1ABC" 
-- run digits "12BC"
-- run digits "123C"
-- run digits "1234"

-- run digits "ABC"

parseInt :: Parser Integer
parseInt = readInt <!> digits

-- match a parser zero or one time
optional :: Parser a -> Parser (Maybe a)
optional pa = Just <!> pa <|> returnP Nothing

-- match a digit followed by an optional semicolon
digitThenSemiColon = digit .>>. optional (pchar ';')
-- run digitThenSemiColon "9;2" == Success ('9', Just ';') "2"

parseIntMinus :: Parser Integer
parseIntMinus = toInt <!> (optional (pchar '-') .>>. parseInt)
 where
  toInt (Nothing, num) = num
  toInt (Just '-', num) = (-num)

(>>.) :: Parser a -> Parser b -> Parser b
(>>.) pa pb = snd <!> (pa .>>. pb)     

(.>>) :: Parser a -> Parser b -> Parser a
(.>>) pa pb = fst <!> (pa .>>. pb)      

-- improved digitThenSemicolon 
-- before digitThenSemicolon = digit .>>. (optional pchar ';')
digitThenSemicolon1 = digit .>> optional (pchar ';')

-- the following code creates a parser that looks
-- for "AB" followed by one or more whitespaces
-- followed by "CD"

manySpaces = many1 (pchar ' ')

ab_spaces_cd = pstring "AB" .>>. (manySpaces >>. pstring "CD")

-- parse values between delimiters such as quotes or brackets
between :: Parser a -> Parser b -> Parser c -> Parser b
between pa pb pc = pa >>. pb .>> pc

-- parse a quoted integer
-- run (between (pchar '"') parseInt (pchar '"')) "\"100\"" == Success 100 ""

-- parse for instance a list of number separated by commas
sepBy1 pa sep =
 (\ (x,xs) -> x:xs) <!> (pa .>>. many (sep >>. pa))

sepBy pa sep = sepBy1 pa sep <|> (returnP [])

comma = pchar ','
oneOrMoreDigitList = sepBy1 digit comma

-- run oneOrMoreDigitList "1A" == Success "1" "A"
-- run oneOrMoreDigitList "1,2;A" == Success "12" ";A"
-- run oneOrMoreDigitList "1,2,3;" == Success "123" ";"
-- run oneOrMoreDigitList "Z;" == Failure expecting 9 got Z



-- BIND
bindP :: (a -> Parser b) -> Parser a -> Parser b
bindP f pa =
   let
    fun chars =
      case runOnInput pa chars of
       Failure  l e p -> Failure  l e p
       Success x xs -> runOnInput (f x) xs  
   in Parser {parseFn = fun, label = "unkown"}

(>>==) pa f = bindP f pa



data Person = Person 
 { name :: String
 , edad :: Integer
}

whitespaceChar = satisfy (\x-> x == ' ') "whitespace"


-- Finally after many days, JSON parser :)
data JValue =
   JString String
 | JNumber Integer
 | JBool Bool
 | JNull
 | JObject (M.Map String JValue)
 | JArray [JValue]
  deriving Show

runPrint :: Show a => Parser a -> String -> String
runPrint p = printResult . run p

-- parse JSON null
jNull:: Parser JValue
jNull = (<?> "null") $ (\ _ -> JNull) <!> pstring "null"

-- parse JSON bool
jBool:: Parser JValue
jBool = 
 let
   pTrue  = (\ _ -> JBool True)  <!> pstring "true"
   pFalse = (\ _->  JBool False) <!> pstring "false"
 in (<?> "bool") pTrue <|> pFalse 

-- parse JSON string
jUnescapedChar:: Parser Char
jUnescapedChar = 
  satisfy (\x -> x /= '\\' && x /= '\"') "char"

jEscapedChar:: Parser Char
jEscapedChar =
  let 
   toParser = \ (toMatch, result) ->
     (\ _ -> result) <!>  pstring toMatch
   list =
     [ ("\\\"", '\"')   -- quote
     , ("\\\\", '\\')   -- reverse solidus
     , ("\\/",  '/' )   -- solidus
     , ("\\b",  '\b')   -- backspace
     , ("\\f",  '\f')   -- formfeed
     , ("\\n",  '\n')   -- newline
     , ("\\r",  '\r')   -- cr
     , ("\\t",  '\t')   -- tab
     ]   
  in 
    choice (toParser <$> list) <?> "escaped char"

--jUnicodeChar:: Parser String
jUnicodeChar =
 let
  backslash = pchar '\\'
  uChar     = pchar 'u'
  hexDigit  = anyOf $ ['0'..'9'] ++ ['A'..'F'] ++ ['a'..'f']
  fourHex   = hexDigit .>>. hexDigit .>>. hexDigit .>>. hexDigit
  convertToChar (((h1,h2),h3),h4) =
   C.chr $ fromInteger $ fromHexString (h1:h2:h3:h4:[])
 in
  convertToChar <!> (backslash >>. uChar >>. fourHex)
 
fromHexString :: String -> Integer
fromHexString chars =
  let
   base10 x 
    |x == '0' =  0 |x == '1' =  1
    |x == '2' =  2 |x == '3' =  3
    |x == '4' =  4 |x == '5' =  5
    |x == '6' =  6 |x == '7' =  7
    |x == '8' =  8 |x == '9' =  9
    |x == 'A' = 10 |x == 'B' = 11
    |x == 'C' = 12 |x == 'D' = 13
    |x == 'E' = 14 |x == 'F' = 15
   convert char power =
    16 ^ power * base10 char
  in 
   sum $ zipWith convert chars (reverse [0..(length chars - 1)]) 

quotedString =
 let
  quote = pchar '\"' <?> "quote"
  jchar = jUnescapedChar <|> jEscapedChar <|> jUnicodeChar
 in
  quote >>. many jchar .>> quote 

