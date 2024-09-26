
type Reducer = [Int] -> Int -> [Int]


operations :: [Reducer -> Reducer]
operations = [ myMap (+3) , myMap (* 5), myMap (+9)]


transducer [] f = f
transducer (f:fs) reducer = transducer fs (\r -> (f r) reducer)

start =  (\acc curr -> acc ++ [curr])

a :: [Int] -> [Int]
a = foldl (transducer operations start) []






myMap f reducer = \ acc curr ->
    reducer acc (f curr)

myFilter p reducer = \ acc curr ->
     if p curr then reducer acc curr else acc 
