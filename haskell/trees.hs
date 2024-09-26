import Control.Monad.State

data Tree a = Node a (Tree a) (Tree a) | Nil
 deriving Show

sample = Node 1 (Node 2 (Node 4 Nil Nil) (Node 5 Nil Nil)) (Node 3 (Node 6 Nil Nil) Nil)

{--
       1
     /   \
    2     3
   / \   /
  4  5  6
--}

myFold :: Tree a -> (b -> a -> b) -> b -> b
myFold Nil _ acc = acc
myFold (Node x l r) f acc =
  myFold r f (f acc' x)
   where acc' = myFold l f acc


myFold' :: Tree a -> (b -> a -> b) -> State b ()
myFold' Nil _ = return ()
myFold' (Node x l r) f = do
    myFold' l f 
    modify (\acc -> f acc x)
    myFold' r f

innerProduct :: ([Int],[Int]) -> Int
innerProduct = sum . map mult . makePairs

(|>) :: a -> (a -> b) -> b
x |> f = f x

innerProduct' x =
  x 
  |> makePairs
  |> map mult
  |> sum




makePairs = (zip <$> fst <*> snd)
mult (a,b) = a * b
