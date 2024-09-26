module OuterInner where

import Control.Monad
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Functor.Identity

embedded1 :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded1 = return 1 

embedded2 :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded2 = MaybeT $  ExceptT  $  ReaderT $ const $ return $ Right $ Just 1

rDec :: Num a => Reader a a
--rDec = ReaderT $ \r ->  return (r-1)
rDec = ReaderT $ return . subtract 1


rShow :: Show a => ReaderT a Identity String
rShow = ReaderT $ return . show

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = ReaderT $ \r -> do
                          print $ "hi : " ++ show r
                          return $ r + 1

sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = StateT $ \s -> do
                          print $ "hi : " ++ show s
                          return (show s, s+1)


isValid v = '!' `elem` v

maybeExcite :: MaybeT IO String
maybeExcite = MaybeT $ do
                v <- getLine
                guard $ isValid v
                return $ Just v

maybeExcite2:: MaybeT IO String
maybeExcite2 = MaybeT $ do
                v <- getLine 
                guard $ isValid v
                runMaybeT $ return v


doExcite :: IO ()
doExcite = do
   putStrLn "say something excite!"
   excite <- runMaybeT maybeExcite
   case excite of
    Nothing -> putStrLn "MOAR EXCITE"
    Just e -> putStrLn ("Good, was very excite: " ++ e)


