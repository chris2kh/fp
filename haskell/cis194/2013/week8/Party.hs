
module Party where
import Employee
import Data.Tree
import Data.Monoid

glCons :: Employee -> GuestList -> GuestList
glCons e (GL es f) = GL (e:es) (empFun e + f)

instance Semigroup GuestList where
 (GL es1 f1) <> (GL es2 f2) = GL (es1 ++ es2) (f1+f2)

instance Monoid GuestList where
 mempty = GL [] 0

moreFun :: GuestList -> GuestList -> GuestList
moreFun g1 g2 | g1 > g2 = g1 
              | otherwise = g2

-- exercise 2
{-
treeFold :: ([b] -> a -> b) -> Tree a -> b
treeFold f (Node a []) = f [] a
treeFold f (Node a xs) = f (treeFold f <$> xs) a
-}


treeFold :: Semigroup b => b -> (b -> a -> b) -> Tree a -> b
treeFold e f (Node a []) = f e a
treeFold e f (Node a xs) = f (foldr (<>) e (treeFold e f <$> xs)) a


{-
treeFold :: (b -> a -> b) -> b -> Tree a -> b
treeFold f init tree = foldl fTree start (subForest tree)
            where fTree = treeFold f
                  start = f init (rootLabel tree)
-}

-- exercise 3
nextLevel :: Employee -> 
             [(GuestList, GuestList)] -> 
             (GuestList, GuestList)

nextLevel boss lists = (withBoss, noBoss) where
 withBoss = foldl mappend (glCons boss mempty) $ snd <$> lists
 noBoss   = foldl mappend mempty $ fst <$> lists

nextLevel' :: Employee -> 
             [(GuestList, GuestList)] -> 
             (GuestList, GuestList)

nextLevel' boss lists = (withBoss, noBoss) where
 withBoss = glCons boss $ foldMap snd lists
 noBoss   = foldMap fst lists

-- hay una funcion poderosa que se llama foldmap
-- aca quise implementarla para entender como se podría implementar

myFoldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
myFoldMap f t = foldr (\el acc -> f el <> acc) mempty t 


-- exercise 4

maxFun :: Tree Employee -> GuestList
maxFun tree = moreFun withBoss withoutBoss
            where results = getGuestList tree
                  withBoss = fst results
                  withoutBoss = snd results

getGuestList :: Tree Employee -> (GuestList, GuestList)
getGuestList tree = nextLevel (rootLabel tree) (map getGuestList $ subForest tree)

-- exercise 5
main = do
     content <- readFile "company.txt"
     let tree = (read content):: Tree Employee
     putStrLn $ "Total fun " ++ show (totalFun tree)
     printNames $ sort $ toString tree


toString :: Tree Employee -> [String]
toString t = treeFold [] (\ acc el -> (empName el):acc) t

sort :: [String] -> [String]
sort [] = []
sort (x:xs) =    sort ([a | a <- xs, a <=x])
              ++ [x] 
              ++ sort ([a| a <- xs, a > x])

printNames:: [String] -> IO ()
printNames [] = do return ()
printNames (x:xs) = do
  putStrLn x
  printNames xs




totalFun :: Tree Employee -> Fun
totalFun = getSum . treeFold (Sum 0) (\acc el -> Sum (getSum acc + empFun el))
