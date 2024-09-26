
permu :: Eq a => [a] -> [[a]]
permu [] = [[]]
permu xs = do
  x      <- xs
  result <- permu $ filter ((/=) x) xs
  return (x:result)

