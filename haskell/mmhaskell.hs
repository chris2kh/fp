module MMhaskell where 

import Data.Char

newtype MaybeT m a = MaybeT (m (Maybe a))
runMaybeT :: MaybeT m a -> m (Maybe a)
runMaybeT (MaybeT x) = x

instance Functor m => Functor (MaybeT m) where
 -- fmap :: (a -> b) -> m a -> m b
 fmap f mta = MaybeT $ (f <$>) <$> runMaybeT mta

instance Applicative m =>  Applicative (MaybeT m) where
  pure =  MaybeT . pure . pure
  -- m (a -> b) -> m a -> m b
  mtf <*> mta = MaybeT $ pure helper <*> runMaybeT mtf <*> runMaybeT mta 
   where
      helper mf ma = mf <*> ma

instance Monad m => Monad (MaybeT m) where
  return = pure
  -- >>= :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  mta >>= f =  MaybeT $ do
            ma <- runMaybeT mta
            case ma of
                    Nothing -> pure Nothing 
                    Just x -> runMaybeT $ f x

main2 :: IO ()
main2 = do
  maybeCreds <- runMaybeT $ do
    usr     <- readUserName'
    email   <- readEmail'
    pass    <- readPassword'
    return (usr,email,pass)
  case maybeCreds of

    Nothing -> print "Couldn't login!"
    Just x -> print x

   readUserName' >>= (\ usr ->
   readEmail'    >>= (\ email ->
   readPassword' >>= (\ pass  ->
    return (usr,email,pass))))

  ma >>= f = 
         case ma of
           Nothing -> Nothing
           Just x   -> f x




askPassphrase1 :: IO ()
askPassphrase1 = do 
               putStrLn "Insert your new passphrase:"
               res <-  ( getPassphrase1 >>= (\a -> return a)) 
               case res of 
                  Nothing  -> putStrLn "Passphrase invalid"
                  Just val -> putStrLn "Storing in database"


getPassphrase1 :: MaybeT IO String
getPassphrase1 = MaybeT $ do
                  s <-getLine
                  if isValid s then return $ Just s
                               else return Nothing


readUserName' :: MaybeT IO String
readUserName' = MaybeT $ do
  putStrLn "Please enter your Username!"
  str <- getLine
  if length str > 5
    then return $ Just str
    else return Nothing

readEmail' :: MaybeT IO String
readEmail' = MaybeT $ do
  putStrLn "Please enter your Email!"
  str <- getLine
  if length str > 5
    then return $ Just str
    else return Nothing

readPassword' :: MaybeT IO String
readPassword' = MaybeT $ do
  putStrLn "Please enter your Password!"
  str <- getLine
  if length str < 8 || null (filter isUpper str) || null (filter isLower str)
    then return Nothing
    else return $ Just str







login u e p = do
          putStrLn $ u ++ " " ++ e ++ " " ++ p
------------


getPassphrase :: IO (Maybe String)
getPassphrase = do s <- getLine
                   if isValid s then return $ Just s
                                else return Nothing

-- The validation test could be anything we want it to be.
isValid :: String -> Bool
isValid s = length s >= 8
            && any isAlpha s
            && any isNumber s
            && any isPunctuation s


askPassphrase :: IO ()
askPassphrase = do putStrLn "Insert your new passphrase:"
                   maybe_value <- getPassphrase
                   case maybe_value of
                       Just value -> do putStrLn "Storing in database..."
                       Nothing -> putStrLn "Passphrase invalid."



---------------------------------

--newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }


