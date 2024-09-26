module Golf where

skips :: [a] -> [[a]]
skips xs = makeLists xs 1

makeLists :: [a] -> Int -> [[a]]
makeLists []  _ = []
makeLists [x] _ = [[x]]
makeLists l@(x:xs) n = (l `every` n): (makeLists xs (n + 1))


every :: [a] -> Int -> [a]
xs `every` nth =
  [ x | (x,i) <- zip xs [0..], i `mod` nth == 0]

-------------------

localMaxima :: [Integer] -> [Integer]
localMaxima (x1:x2:x3:xs)
    | x1 <= x2 && x2 >= x3 = x2: localMaxima (x3:xs)
    | otherwise = localMaxima (x2:x3:xs)
localMaxima _ = []

--------------------

histogram :: [Integer] -> IO ()
histogram xs = putStrLn $ draw $ build xs emptyHistogram

build :: [Integer] -> Histogram -> Histogram
build [] h = h 
build [x] h = incr x h
build (x:xs) h = build (xs) (incr x h)  

data Histogram = Histogram
 { zero  :: Integer
 , one   :: Integer
 , two   :: Integer
 , three :: Integer
 , four  :: Integer
 , five  :: Integer
 , six   :: Integer
 , seven :: Integer
 , eight :: Integer
 , nine  :: Integer
 } deriving (Show)

emptyHistogram = Histogram 0 0 0 0 0 0 0 0 0 0

incr :: Integer -> Histogram -> Histogram
incr 0 h = h { zero  =  (zero  h + 1)  }
incr 1 h = h { one   =  (one   h + 1)  }
incr 2 h = h { two   =  (two   h + 1)  }
incr 3 h = h { three =  (three h + 1)  }
incr 4 h = h { four  =  (four  h + 1)  }
incr 5 h = h { five  =  (five  h + 1)  }
incr 6 h = h { six   =  (six   h + 1)  }
incr 7 h = h { seven =  (seven h + 1)  }
incr 8 h = h { eight =  (eight h + 1)  }
incr 9 h = h { nine  =  (nine  h + 1)  }

get :: Integer -> Histogram -> Integer
get 0 h = zero  h 
get 1 h = one   h 
get 2 h = two   h 
get 3 h = three h 
get 4 h = four  h 
get 5 h = five  h 
get 6 h = six   h 
get 7 h = seven h 
get 8 h = eight h 
get 9 h = nine  h 


toList :: Histogram -> [Integer]
toList h =
  (\f -> f h) <$> 
  [ zero
  , one
  , two
  , three
  , four
  , five
  , six
  , seven
  , eight
  , nine
  ]

draw :: Histogram -> String
draw h =
   [ draw' x y | y <- [biggest, (biggest-1)..1], x <- [0..10] ]
   ++ base
   where
     biggest = foldl1 max $ toList h
     draw' 10 _ = '\n'
     draw' x y
       | get x h >= y  = '*'
       | otherwise = ' '

base :: String
base = "==========\n0123456789\n"
