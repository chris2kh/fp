-- exercise 1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = fib <$> [0..]

-- exercise 2
next :: Integer -> Integer -> [Integer]
next  a b = a: next b (a+b)

fibs2' = next 0 1 

-- esto lo vi en el cis194 de Joachim. Genio!
fibs2 :: [Integer]
fibs2 = 0:1:zipWith (+) fibs2 (tail fibs2)

data Stream a = Cons a (Stream a) 

instance Show a => Show (Stream a) where
  show = first 20 where
         first 0 _ = "...."
         first n (Cons x s) = show x ++ "," ++ first (n-1) s

makeStream :: Integer -> Stream Integer
makeStream x = Cons x (makeStream (x+1)) 

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x:streamToList xs

-- exercise 4
streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x s) = Cons (f x) (streamMap f s)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f (f x))

-- exercise 4
nats :: Stream Integer
nats = streamFromSeed (+1) 0

-- exercise 5
zeros = streamRepeat 0
evens :: Stream Integer
evens = streamFromSeed (+2) 2

streamInterleave :: Stream a -> Stream a -> Stream a
streamInterleave (Cons x xs) ys =
  Cons x (streamInterleave ys xs)

ruler :: Stream Integer
ruler = rulerHelper 0
   where
   rulerHelper x = streamInterleave (streamRepeat x) (rulerHelper (x+1))
