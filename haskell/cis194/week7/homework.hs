module Cristian where
 
 fib :: Integer -> Integer
 fib 0 = 1
 fib 1 = 1
 fib n = fib (n-1) + fib (n-2)
 
 fibs1 :: [Integer]
 fibs1 = map fib [0..]
 
 fibs2 :: [Integer]
 fibs2 = 1:1: spread
   where spread = zipWith (+) fibs2 (tail fibs2)


-- exercise 2 streams
 data Stream a = Cons a (Stream a)
 
 -- TODO: leer bien como funciona el overloading (uso de instance)
 instance Show a => Show (Stream a) where
  show = go 20 
   where 
     go 0 (Cons x _)  = show x ++ "..."
     go n (Cons x xs) = show x ++ "," ++ go (n-1) xs 
  
 
 nums :: Stream Integer
 nums = nums' 1 
  where 
   nums' x = Cons x ( nums' (x+1))
 
 streamRepeat:: a -> Stream a
 streamRepeat x = Cons x (streamRepeat x) 
  
 streamToList :: Stream a -> [a]
 streamToList (Cons x xs) = x:streamToList xs
 
 streamMap :: (a -> b) -> Stream a -> Stream b
 streamMap f (Cons x xs) =
        Cons (f x) (streamMap f xs)
  
 streamIterate :: (a -> a) -> a -> Stream a
 streamIterate f x = (Cons (f x) (streamIterate f (f x)))

 streamInterleave :: Stream a -> Stream a -> Stream a
 streamInterleave (Cons x xs) y =
           Cons x (streamInterleave y xs)

 -- me toco ver la respuesta porque no fui capaz de resolverlo
 ruler :: Stream Integer
 ruler = streamInterleave (streamRepeat 0) (streamMap (+1) ruler)

-- recordar dejar el espacio de la margen

 data Supply s a = S (Stream s -> (a, Stream s))
 {--
 idiom1 :: Supply s a
 idiom1 S (\xs -> (a, xs))

 idiom2 :: Supply s a
 idiom2 = S go
   where go xs = (a, xs)
--}
 get :: Supply s s
 get = S (\ (Cons x xs) -> (x,xs))

 pureSupply :: a -> Supply s a
 pureSupply x = S (\xs -> (x,xs))

 mapSupply :: (a -> b) -> Supply s a -> Supply s b
 mapSupply f (S fa) = 
   S fb
    where 
     fb xs = (f a, xs') 
             where 
                (a, xs') = fa xs

 mapSupply2 :: (a -> b -> c) -> Supply s a -> Supply s b -> Supply s c
 mapSupply2 f (S fa) (S fb) =
     S fc
      where
        fc xs = (f a b, xs_after_b)
           where 
               (b, xs_after_b) = fb xs_after_a
               (a, xs_after_a) = fa xs 
-- 18 feb 2022: el resto de la tarea me dio pereza terminarla porque ese
-- tema de supply monad no lo entendi bien y me aburri jaja
