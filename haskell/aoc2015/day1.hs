{-# LANGUAGE LambdaCase #-}
module Day1 where
import Utils
import Control.Monad 

main :: IO ()
main = do
 input <- parseString "inputs/day1.txt"
 print $ whatFloor input
 print $  input
       |> map toNum
       |> partialSums
       |> takeWhile (/= -1)
       |> length
       |> (+ 1)
 print $ anotherSol2 (toNum <$> input) -- alternative to start practicing
                            -- control monad stuff

whatFloor :: String -> Int
whatFloor = foldr ( (+) . toNum) 0

toNum :: Char -> Int
toNum = \case
  '('  ->  1
  ')'  -> -1
  '\n' ->  0
  s    ->  error $ "found symbol that is not ( or ) : " ++ show s

partialSums =
 go 0 where 
 go _ []       = []
 go acc (x:xs) = let 
                 acc' = acc + x
                 in acc':go acc' xs

anotherSol2 :: [Int] -> Either Int (Int, Int)
anotherSol2 = foldM try (1,0) 
  where try (pos,acc) num = 
         let
          acc' = acc + num in
          if acc' == -1 
             then Left pos
             else Right (pos + 1, acc')
