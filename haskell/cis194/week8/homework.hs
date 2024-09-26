data ComplicatedA a b =
  Con1 a b 
  | Con2 [Maybe (a -> b)]

instance (Show a, Show b) => Show (ComplicatedA a b) where
 show (Con1 a b) = (show "Con1 ") ++ (show a) ++ "," ++ (show b) 
 show (Con2 [Just f]) = show "Con2...."


instance Functor (ComplicatedA a)  where
  fmap f (Con1 a b) = Con1 a (f b)
  fmap f (Con2 [Just g]) = Con2 [Just (f.g)]


data ComplicatedB f g a b 
 = Con3 (f a)
 | Con4 (g b)
 | Con5 (g (g [b]))


instance Functor g => Functor (ComplicatedB f g a) where
 fmap f' (Con3 fa) =  Con3 fa -- este no lo entendi
 fmap f' (Con4 gb) =  Con4 (fmap f' gb)
 fmap f' (Con5 ggb) =  Con5  (fmap (fmap (map f')) ggb)

-- exercise 2 Rewriting monadic code
-- 2.0
func0 :: Monad m => (a -> a) -> m a -> m a
func0 f xs = do
 x <- xs
 return $ f $ f x

func0' :: Functor f => (a -> a) -> f a -> f a
func0' fun xs = (fun . fun) <$> xs

-- 2.1
func1 :: Monad m =>  m a -> m (a, a)
func1 xs = xs >>= (\x -> return (x,x)) 

func1':: Functor f => (a -> (a,a)) -> f a -> f (a, a)
func1' f xs = f <$> xs

-- 2.2
func2 :: Monad m => m a -> m (a,a)
func2 xs = xs >>= (\x -> xs >>= \y -> return (x,y))

func2':: Functor f => (a -> (a,a)) -> f a -> f (a, a)
func2' f xs = (f . id) <$> xs

-- 2.3
func3 :: Monad f => f a -> f (a,a)
func3 xs = xs >>= (\x -> xs >>= \y -> return (x,x))

func3':: Functor f => (a -> (a,a)) -> f a -> f (a, a)
func3' f xs = (f . id) <$> xs

-- 2.4
func4 :: Monad f => f a -> f a -> f (a,a)
func4 xs ys = xs >>= (\x -> ys >>= \y -> return (x,y))

--func4' Functor f => f a -> f a -> (a -> a -> (a,a))
-- creo que se puede pero toca implementar join para aplanar dos fmaps anidados 

-- 2.5
func5 :: Monad f => f Integer -> f Integer -> f Integer
func5 xs ys = do
    x <- xs
    let x' = x + 1
    y <- (+1) <$> ys
    return (x' + y)

--func5 creo no se puede con functors

func6 :: Monad f => f Integer -> f (Integer,Integer)
func6 xs = do
    x <- xs
    return $ if x > 0 then (x, 0)
                      else (0, x)

func6' :: Functor f => f Integer -> f (Integer, Integer)
func6' f =
   (\ x -> if x > 0 then (x,0) else (0, x)) <$> f

-- 2.7
func7 :: Monad f => f Integer -> f (Integer,Integer)
func7 xs = do
    x <- xs
    if x > 0 then return (x, 0)
             else return (0, x)

func7' :: Functor f => f Integer -> f (Integer, Integer)
func7' f =
   (\ x -> if x > 0 then (x,0) else (0, x)) <$> f
   

-- 2.8
func8 :: Monad f => f Integer -> Integer -> f Integer
func8 xs x = pure (+) <*> xs <*> pure x

func8' :: Applicative f => f Integer -> Integer -> f Integer
func8' xs x = pure (+) <*> xs <*> pure x

-- 2.9
func9 :: Monad f => f Integer -> f Integer -> f Integer -> f Integer
func9 xs ys zs = xs >>= \x -> if even x then ys else zs

func9' :: Applicative f => f Integer -> f Integer -> f Integer -> f Integer
func9' a b c = 
  pure (\ a b c -> if even a then b else c) <*> a <*> b <*> c

-- 2.10
func10 :: Monad f => f Integer -> f Integer
func10 xs = do
    x <- xs >>= (\x -> return (x * x))
    return (x + 10)

func10' :: Functor f => f Integer -> f Integer
func10' xs = (\x -> x*x + 10) <$> xs

func10'' :: Applicative f => f Integer -> f Integer
func10'' xs = pure (\x -> x*x + 10) <*> xs


-- Excercise 3: A parser monad

data Result a = MyNothing | Result a String deriving Show
data Parser a = Parser (String -> Result a)

runParser :: Parser a -> (String -> Result a)
runParser (Parser p) = p

parse :: Parser a -> String -> Maybe a
parse (Parser f) s =
 case f s of
   MyNothing -> Nothing
   Result a "" -> Just a
   Result a _ -> Nothing

noParser :: Parser a
noParser = Parser (\ _ -> MyNothing)

pureParser :: a -> Parser a
pureParser = \x -> Parser (\ s -> Result x s)


instance Functor Parser where
 fmap f (Parser p) = 
   Parser ( \s -> case (p s) of     
       MyNothing -> MyNothing
       Result a s -> Result (f a) s )
{--
 recordando que 
  f :: a -> b
  p :: Parser a
  fmap :: (a -> b) -> Parser a -> Parser b


  1.  parse (fmap f p) input  == 2. fmap f (parse p input)

  1. parserar un input con un parser mappeado de Parser a a Parser b
   es igual a 
  2. parsear un input con parser a a Result a, y mapear luego eso a
     Result b
--}

instance Applicative Parser where
 pure = pureParser
 (Parser pf) <*> (Parser px) =
  
  Parser (
    \s -> 
         case (pf s) of
          MyNothing -> MyNothing
          Result f s -> 
                 case (px s) of
                  MyNothing -> MyNothing
                  Result x s' -> Result (f x) s'
  )

instance Monad Parser where
 return = pureParser
 (Parser p) >>= k =
   Parser (
     \s ->
       case (p s) of
        MyNothing -> MyNothing
        Result a s -> runParser (k a) $ s
   )

anyChar :: Parser Char
anyChar = 
  Parser (
   \s ->
     let inspect "" = MyNothing 
         inspect (x:xs) = Result x xs
     in inspect s 
  )

char :: Char -> Parser ()
char c = 
   Parser (
     \s -> helper s c
   )

helper xs c = 
     case xs of
       (x:y) -> if x == c then Result () y else MyNothing
       _    -> MyNothing


anyCharBut :: Char -> Parser Char
anyCharBut c =
  Parser (
     \s -> helper2 s c
  )

helper2 xs c =
   case xs of 
     (x:y) -> if x /= c then Result x y else MyNothing
     _ -> MyNothing


orElse :: Parser a -> Parser a -> Parser a
orElse p1 p2 =
   Parser (
    \s ->
      case runParser p1 s of 
        Result x xs -> Result x xs
        MyNothing -> runParser p2 s
   )

many :: Parser a -> Parser [a]
many p = Parser (collect []) 
 where
   collect acc [] = Result acc ""
   collect acc xs  = 
    case (runParser p xs) of
     Result x xs' -> collect (acc ++ [x]) xs'
     _ -> Result acc xs

sepBy:: Parser a -> Parser () -> Parser [a]
sepBy p1 p2 = Parser (collect [])
  where 
   collect acc [] = Result acc ""
   collect acc xs = 
    case (runParser p1 xs) of
      MyNothing -> Result acc xs
      Result x xs' -> 
       case (runParser p2 xs') of
         MyNothing -> Result (acc ++ [x]) xs'
         Result ()  xs'' -> collect (acc ++ [x]) xs''

parseCSV :: Parser [[String]]
parseCSV = many parseLine
  where
    parseLine = parseCell `sepBy` char ','
    parseCell = do
        char '"'
        content <- many (anyCharBut '"')
        char '"'
        return content

