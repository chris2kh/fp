facto x = case x of
  0 -> 0
  x | x < 0 -> 1
  _ -> 2
