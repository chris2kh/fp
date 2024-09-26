newtype ZipList a = ZipList { getZipList :: [a] }
  deriving (Eq, Show)


instance Applicative ZipList where
  pure = ZipList . repeat
  ZipList fs <*> ZipList xs = ZipList (zipWith ($) fs xs)

instance Functor ZipList where
 fmap f xs = ZipList $ f <$> getZipList xs 
