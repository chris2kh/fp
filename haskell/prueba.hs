newtype State s a = State { runState :: (s -> (a,s)) }



newtype Ss s a = Ss (s -> (a, s))
run :: Ss s a -> s -> (a, s)
run (Ss f) s = f s


instance Functor (Ss s) where
 f `fmap` ssa = Ss $ \s ->
     let (a, s1) = run ssa s
     in ( f a, s1)

instance Applicative (Ss s) where
 pure x = Ss $ \s -> (x, s)
 ssf <*> ssa = Ss $ \s -> 
     let (f, s1) = run ssf s
         (a, s2) = run ssa s1
     in (f a, s2)


oe :: Ss String Int
oe = Ss $ \state -> (2, state)

oe2 :: Ss String (Int -> Int)
oe2 = Ss $ \(x:xs) -> ((+10), xs)

oe3 :: Ss String Int
oe3 = Ss $ \(x:xs) -> (20, xs ++ xs ++ "hola")

oe4 :: Int -> Ss String Int
oe4 n = Ss $ \(x:xs) -> ( n + 9, xs ++ [x])


exercise :: Ss String Int
exercise = do
    x <- oe
    y <- oe4 x
    return (x + y)

exercise1 :: Ss String Int
exercise1  =
       oe    >>=    \x ->
       oe4 x >>=    \y ->
       return (x + y)
     
getState :: Ss s s
getState = Ss $ \s -> (s, s)


putState :: s -> Ss s ()
putState s = Ss $ \_ -> ((), s)

findLargest :: [Int] -> Ss Int ()
findLargest [] = return ()
findLargest (x:xs) = do
       largest <- getState
       if (x > largest)
        then
         putState x
        else
         putState largest
       findLargest xs 
          

findLargest1 :: [Int] -> Ss Int ()
findLargest1 [] = return ()
findLargest1 (x:xs) = 
       getState >>= 
              \largest ->
               (
                 if (x > largest) then putState x
                 else putState largest
               ) >>= (\ _ ->
                       findLargest xs)


instance Monad (Ss a) where
  return = pure
  ssa >>= f = Ss $ \s ->
    let (a, s1) = run ssa s
    in run (f a) s1








