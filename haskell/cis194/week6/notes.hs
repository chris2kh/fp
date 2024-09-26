myListBind :: [a] -> (a -> [b]) -> [b]
myListBind [] _ = []
myListBind (x:xs) f =
  (f x) ++ myListBind xs f

mapM1 :: Monad m => (a -> m b) -> [a] -> m [b]
mapM1 f [] = return []
mapM1 f (a:as) = f a >>= \b -> mapM1 f as >>= \bs -> return (b:bs)



