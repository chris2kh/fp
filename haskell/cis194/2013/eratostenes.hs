

primes :: [Integer]
primes = sieve [3,5..]

sieve :: [Integer] -> [Integer]
sieve (x:xs) =
    x: sieve ( filter (\n -> n `mod` x /= 0) xs)
