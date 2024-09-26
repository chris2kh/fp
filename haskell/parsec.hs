import Text.ParserCombinators.Parsec

data Claim = 
 Claim 
  { id           :: Int
  , paddingLeft  :: Int
  , paddingTop   :: Int
  , width        :: Int
  , height       :: Int
  } deriving Show

integer = read <$> many1 digit

claim :: Parser Claim
claim = do
     char '#'
     id <- integer
     
     string " @ "
     pl <- integer
     
     char ','
     pt <- integer
     
     string ": "
     w <- integer
     
     char 'x'
     h <- integer
     return $ Claim id pl pt w h

main :: IO ()
 
