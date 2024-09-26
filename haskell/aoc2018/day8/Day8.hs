module Day8 where



main :: IO ()
main = do
 contents <- parseFile "day8Input.txt"
 --contents <- parseFile "test_input.txt"
 let (node, _)  = myParse contents
 
 print $ collect node
 print $ getValue node

collect :: Tree Int -> Int
collect (Node _ myMetadata children) =
    sum myMetadata + (sum $ collect <$> children)

getValue :: Tree Int -> Int
getValue (Node 0 metadata _) =
     sum metadata

getValue (Node n metadata children) =
  sum $ sumChildren  <$>  metadata
  where
   sumChildren index
    | index < 1 || index > n = 0
    | otherwise = getValue $ children !! (index -1)

data Tree a = Node a [a] [Tree a]
 deriving Show 
     
myParse :: [Int] -> (Tree Int, [Int])
myParse [] = error "cannot parse empty input"
myParse (0:n:xs) =
      let metadata = take n xs
          rest     = drop n xs
      in (Node 0 metadata [], rest)
myParse (x:n:xs) =
      let (children, rest) = helperParse x [] xs
          metadata         = take n rest
          rest'            = drop n rest
      in (Node x metadata children, rest')


helperParse :: Int -> [Tree Int] -> [Int]-> ([Tree Int], [Int])
helperParse 0 children rest = (children, rest)
helperParse n siblings input =
        let (sibling, rest) = myParse input
        in helperParse (n-1) (siblings ++ [sibling]) rest 




parseFile :: String -> IO [Int]
parseFile filename = 
    readFile filename >>= (return . (read <$>) . words)
