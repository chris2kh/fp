main = do
   putStrLn "Greetings! What is your name?"
   name <- getLine
   putStrLn $ "Welcome to Haskell " ++ name ++ " !"
   let a = double 5
   putStrLn $ "hola" ++ show a

double :: Integer -> Integer
double = (*2)
