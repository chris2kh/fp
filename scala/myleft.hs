

-- foldr :: (a -> b -> a) -> b -> [a] -> b
myfoldr :: (a -> b -> b) -> b -> [a] -> b
myfoldr _ b [] = b
myfoldr f acc (x:xs) = x `f` (myfoldr f acc xs)


myfoldl :: (b -> a -> b) -> b -> [a] -> b
myfoldl _ b [] = b
myfoldl f acc (x:xs) = myfoldl f (acc `f` x) xs


myfoldl2 _ b [] = b
myfoldl2 f acc (x:xs) = myfoldl2 f (myfoldr (flip f) acc [x]) xs

myfoldr2 f acc xs = foldl helper id xs $ acc
 where helper f' x = \y ->  f  x (f' y)

myfoldl3 f acc xs = foldr (helper f) id xs $ acc
helper :: (b -> a -> b) -> a -> (b -> b) -> (b -> b)
helper  f x f'  = \y ->  f  (f' y) x


factorial 1 callback = callback 1
factorial n callback = factorial (n - 1) (\x ->
                                            callback (n*x))


