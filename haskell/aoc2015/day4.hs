module Day4 where
import Utils
import qualified Data.ByteString.Lazy.Internal as LB
import Data.Digest.Pure.MD5 hiding (hash)
import Data.List (isPrefixOf)

main :: IO ()
main = do
 input <- parseString "inputs/day4.txt"
 
 -- part a
 print $ head  
       $ dropWhile (not . ("00000" `isPrefixOf`) . hash input) [1..]

 -- part b
 print $ head  
       $ dropWhile (not . ("000000" `isPrefixOf`) . hash input) [1..]

hash :: String -> Int -> String
hash secretKey num =
    num
    |> show
    |> (secretKey ++)
    |> LB.packChars
    |> md5
    |> show 
