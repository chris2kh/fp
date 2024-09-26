module Chapter26 where

import Chapter25

data MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance (Functor m) => Functor (MaybeT m) where
  fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

data MyEither a b = Lefty a | Righty b

instance Functor (MyEither a) where
  fmap f (Righty b) = Righty (f b)
  fmap _ (Lefty a)  = Lefty a

instance Applicative (MyEither a) where
  pure x = Righty x
  (<*>) (Lefty fa) _ =  Lefty fa
  (<*>) (Righty fa) (Righty a) = Righty (fa a)
 
instance Monad (MyEither a) where
  return = pure
  Lefty fa >>= _ = Lefty fa
  Righty a >>= f = f a

data EitherT m a b = EitherT { runEitherT :: m (Either a b) } 

instance Functor m => Functor (EitherT m a) where
 fmap f (EitherT mab) = EitherT $ (fmap . fmap) f mab

instance Applicative m => Applicative (EitherT m a) where
 pure = EitherT . pure . pure
 (<*>) (EitherT mf) (EitherT ma) = EitherT $ (<*>) <$> mf <*> ma

instance Monad m => Monad (EitherT m a) where
 return =  pure
 (EitherT ma) >>= f = EitherT $ do
                  either <- ma
                  case either of
                   Left a  -> return $ Left a
                   Right b -> runEitherT $ f b
  
swapEither :: Either a b -> Either b a
swapEither (Left a)  = Right a 
swapEither (Right a)  = Left a

swapEitherT :: (Functor m) => EitherT m a e -> EitherT m e a
swapEitherT (EitherT mae) =  EitherT $ fmap swapEither mae


eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT m a b -> m c
eitherT fa fb (EitherT mea) = mea >>= either fa fb

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance Functor m => Functor (ReaderT r m) where
   fmap fun (ReaderT rma) = ReaderT $ (\r -> fmap fun (rma r))

instance Applicative m => Applicative (ReaderT r m) where
   pure x = ReaderT $ (\_ ->  pure x)
   (ReaderT rab) <*> (ReaderT ra) = ReaderT $ (\r ->
                                      let ma = ra r
                                          mf   = rab r 
                                      in mf <*> ma)



instance Monad m => Monad (ReaderT r m) where
 return = ReaderT . pure . pure
 (ReaderT rma) >>= f = 
          ReaderT $ (\r -> do
                      a  <- rma r
                      (runReaderT $ f a) r) 

-----------------------------------------------

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) } 

instance Functor m => Functor (StateT s m) where
  fmap f stateT = StateT $ \s ->
                       let ma = runStateT stateT s
                       in fmap (\ (a,s) -> (f a, s)) ma

instance Monad m => Applicative (StateT s m) where
  pure x = StateT $ \s -> pure $ (x, s)                     
  (StateT sfun) <*> (StateT sa) = 
     StateT $ \s -> do
                (f,s') <- sfun s
                (a,s'') <- sa s'
                return (f a, s'')
                     
instance Monad m => Monad (StateT s m) where
 return = pure
 (StateT sa) >>= f = StateT $ \s -> do
                       (a, s') <- sa s
                       runStateT (f a) s 









