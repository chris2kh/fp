module Utils
 ( parseString, parseList, (|>))
where

parseString :: String -> IO String
parseString filename = readFile filename 
 >>= (return . reverse . tail . reverse) --remove annoying '\n'

parseList :: String -> IO [String]
parseList filename = readFile filename >>= (return . lines)

(|>) :: a -> (a -> b) -> b
(|>) a f = f a
