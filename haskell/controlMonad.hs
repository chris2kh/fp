import Control.Monad

newtype MyState a b = My (a -> (b,a)) 


run :: MyState a b -> a -> (b,a)
run (My f) a  = f a

instance Monad (MyState a) where
 return x = My $ \state -> (x, state)
 updater >>= f = My $ \state ->
               let (result, newState) = run updater state
                in 
                 run (f result) newState


instance Applicative (MyState a) where
 pure = return
 updater1 <*> updater2 = 
         My $ \state ->
              let (f, newState) = run updater1 state
                  (x, newState')  = run updater2 newState
               in (f x, newState')

instance Functor (MyState a) where
 fmap f updater = My $ \state ->
                   let (x, newState) = run updater state
               in (f x, newState) 

get :: MyState s s
get = My $ \state -> (state, state)

put :: (s -> s) -> MyState s a -> MyState s a
put f stateFn = My $ \s -> 
     let (res, s') = run stateFn s
     in (res, f s')


findLargest :: [Int] -> ((), Int)
findLargest xs =
   run (foldr fun (return ()) xs) 0
   where 
     fun curr updater =
       My $ \state ->
          let (res, s) = run updater state
          in
           if (curr > s) then (res, curr)
                         else (res, s)

findLargestMonad :: [Int] -> ((), Int)
findLargestMonad xs = 
   run (foldr fun (return ()) xs) 0
   where 
     fun curr updater = do
       s <- get
       res <- updater
       put (\s -> if curr > s then curr else s) updater
       return res


findLargestMonad2:: [Int] -> ((), Int)
findLargestMonad2 xs = run (go xs) 0
  where go [] = return ()
        go (x:xs) = do
           put (\currLargest -> 
                if currLargest > x then currLargest
                                   else x)
                (return ())
           go xs
    

append :: Show a => a -> [String] -> [String]
append x logs = ("found: " ++ show x) : logs 

findLargestMonadWithLogs :: [Int] -> ([String] , Int)
findLargestMonadWithLogs xs = 
   run (foldr fun (return []) xs) 0
   where 
     fun x updater = do
         logs <- updater
         largest <- get
         if (x > largest) 
            then 
              put (\_ -> x) 
                  (return $ append x logs) 
            else 
              return logs


findLargestMonadWithLogs2 :: [Int] -> ([String] , Int)
findLargestMonadWithLogs2 xs =
  run (go xs) 0 
   where go [] = return []
         go (x:xs) = do 
             logs <- go xs
             largest <- get 
             if (x > largest) 
               then 
                put (\_ -> x) 
                    (return $ append x logs) 
             else 
              return logs

   



      
    




findLargestWithLogs :: [Int] -> ([String], Int)
findLargestWithLogs xs =
 run (foldr fun (return []) xs) 0
  where 
   fun curr updater =
    My $ \state ->
     let (logs, s) = run updater state
      in
      if (curr > s) 
       then 
        (("new largest found " ++ show curr):logs, curr)
       else 
        (logs, s)


