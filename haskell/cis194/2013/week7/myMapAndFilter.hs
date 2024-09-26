

myMap :: (a -> b) -> [a] -> [b]
myMap f fa = foldr (\ x acc -> f x:acc) [] fa

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p fa = foldr (\ x acc -> if p x then x:acc else acc) [] fa
