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
    fun acc [] = Success acc ""
    fun acc chars =
         case run pa chars of
           Failure _ -> Success acc chars
           Success x xs -> fun (acc ++ [x]) xs
  in Parser $ fun [] 

many1 :: Parser a -> Parser [a]
many1 pa =
  let
   fun chars =
    case run pa chars of
     Failure msg -> Failure msg
     Success x xs -> 
          Success (x:x') xs'
          where Success x' xs' = run (many pa) xs
  in
   Parser fun

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
      case run pa chars of
       Failure msg -> Failure msg
       Success x xs -> run (f x) xs  
   in Parser fun

(>>==) pa f = bindP f pa
