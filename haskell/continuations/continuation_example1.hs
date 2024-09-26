module Continuation1 where

factorial :: Int -> (Int -> Int) -> Int
factorial 1 f =  f 1
factorial x f = factorial (x - 1) (\n -> n * f x)
