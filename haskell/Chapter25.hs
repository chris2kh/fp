module Chapter25 where

newtype Compose f g a = Compose { getCompose :: f ( g a) }

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap fun comp =  Compose $ (fmap . fmap) fun (getCompose comp)


instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure = Compose . pure . pure 
  fun <*> comp =  
       Compose $  pure (<*>) <*> (getCompose fun) <*> (getCompose comp)

--  foldMap :: Monoid m => (a -> m) -> t a -> m
--  foldr :: (a -> b -> b) -> b -> t a -> b

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  --foldMap :: Monoid m => (a -> m) -> Compose f g a -> m
  foldMap fun comp =
        foldMap (foldMap fun) (getCompose comp)

-- sequence :: Monad m => t (m a) -> m (t a)

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
-- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
 traverse fun (Compose fga) = Compose <$> (traverse . traverse) fun fga



class Bifunctor p where
  {-# MINIMAL bimap | first, second #-}
  bimap :: (a -> b)
        -> (c -> d)
        -> p a c
        -> p b d
  bimap f g = first f . second g
  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id
  second :: (b -> c) -> p a b -> p a c
  second = bimap id

data Deux a b = Deux a b
instance Bifunctor Deux where
 first  f1 (Deux a b) = Deux (f1 a) b
 second f2 (Deux a b) = Deux a     (f2 b)
 
data Const a b = Const a
instance Bifunctor Const where
 first f (Const a) = Const (f a)
 second _ cons     = makeConstC cons

makeConstC :: Const a b -> Const a c
makeConstC (Const x) = Const x

data Drei a b c = Drei a b c
instance Bifunctor (Drei a) where
 first f (Drei a b c) = Drei a (f b) c
 second f (Drei a b c) = Drei a b (f c)

data SuperDrei a b c = SuperDrei a b
instance Bifunctor (SuperDrei a) where
 first f (SuperDrei a b) = SuperDrei a (f b)
 second f sp = makeSuperDrei sp

makeSuperDrei :: SuperDrei a b c -> SuperDrei a b d
makeSuperDrei (SuperDrei a b) = SuperDrei a b

data SemiDrei a b c = SemiDrei a
instance Bifunctor (SemiDrei a) where
 first f sd =  makeSemiDrei1 sd 
 second f sd = makeSemiDrei2 sd


makeSemiDrei1 :: SemiDrei a b c -> SemiDrei a d c
makeSemiDrei1 (SemiDrei a) = SemiDrei a

makeSemiDrei2 :: SemiDrei a b c -> SemiDrei a b e
makeSemiDrei2 (SemiDrei a) = SemiDrei a

data Quadriceps a b c d = Quadzzz a b c d
instance Bifunctor (Quadriceps a b) where
 first f (Quadzzz a b c d) = Quadzzz a b (f c) d
 second f (Quadzzz a b c d) = Quadzzz a b c (f d)

instance Bifunctor Either where
 first f (Left a)   = Left $ f a
 second f (Right b) = Right $ f b


-----------------------
newtype Identity a = Identity { runIdentity :: a }

newtype IdentityT f a = IdentityT {runIdentityT :: f a }

instance Functor Identity where
  fmap f (Identity a) = Identity (f a) 

instance Functor f => Functor (IdentityT f) where
  fmap f (IdentityT fa) = IdentityT (fmap f fa)

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity a) = Identity (f a)

instance Applicative f => Applicative (IdentityT f) where
  pure = IdentityT . pure
  (<*>) (IdentityT fun) (IdentityT fa) = IdentityT $ fun <*> fa 

instance Monad Identity where
  return = pure
  (Identity a) >>= f = f a

instance Monad m => Monad (IdentityT m) where
  return = pure
  (IdentityT ma) >>= f = IdentityT $ do
                           a  <-  ma
                           runIdentityT ( f a) 

 
