
greaterThan :: Integer -> Integer -> Integer
greaterThan a b 
 | a >= b = a
 | otherwise = b                


maxFromList :: [Integer] -> Integer
maxFromList = 
 foldr1 (\ max curr -> if curr > max then curr else max) 

isInList :: [Integer] -> Integer -> Bool
isInList [] _ = False
isInList (x:xs) num = 
 x == num || isInList xs num


greater_tests :: (String, [Bool])
greater_tests =
 ( "greater_tests",
 [ greaterThan 10 2  == 10
 , greaterThan 67 5  == 67
 , greaterThan 10 5  == 10
 , greaterThan 1  4  == 4
 , greaterThan 44 10 == 44
 ]
 )


main = do
 putStrLn $ 
  "the results from " ++ fst greater_tests ++ " are: "
   
 let (numGood, numBad) = getResults greater_tests
   
 putStrLn $ 
  "passing tests : " ++ show numGood
   
 if numBad /= 0 
  then return ()
  else putStrLn $ 
        "failing tests : " ++ show numBad


getResults :: (String, [Bool]) -> (Int, Int)
getResults (_,tests) = ( numGood, numBad )
 where numGood = length $ filter id tests
       numBad  = length tests - numGood
