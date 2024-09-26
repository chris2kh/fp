import Control.Monad.State

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show


label :: Tree Char -> Tree Int
label tree = fst $ go tree 0
 where 
    go :: Tree a -> Int -> (Tree Int, Int)
    go (Leaf c)   n = (Leaf n, n+1) 
    go (Node l r) n = (Node l' r', n'')
           where
            (l', n')  =  go l n
            (r', n'') =  go r n'


fresh = 
   get >>= \n ->
   put (n + 1) >> return n

label' :: Tree a -> Tree Int
label' tree = fst $ runState (mlabel tree) 0
   where mlabel (Leaf c) = do
                 n <- fresh
                 return (Leaf n)

         mlabel (Node l r) = do
                 l' <- mlabel l
                 r' <- mlabel r
                 return $ Node l' r'
                 



