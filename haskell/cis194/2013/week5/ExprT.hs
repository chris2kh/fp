module ExprT where

data ExprT = Lit Integer
           | Add ExprT ExprT
           | Mul ExprT ExprT
  deriving (Show, Eq)

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit x | x <= 0 = False | otherwise = True
  add = (||)
  mul = (&&)

data MinMax = MinMax Integer deriving (Show)
instance Expr MinMax where
  lit = MinMax
  add (MinMax a) (MinMax b) = MinMax (max a b)
  mul (MinMax a) (MinMax b) = MinMax (min a b)

data Mod7  = Mod7 Integer deriving (Show)
instance Expr Mod7 where
  lit x 
        | x `elem` [0..6] = Mod7 x
        | otherwise = error "number must be between 0 and 6!"
  add (Mod7 x) (Mod7 y) = Mod7 ((x + y) `mod` 7)
  mul (Mod7 x) (Mod7 y) = Mod7 ((x * y) `mod` 7)
  


doubleIt :: Expr a => a -> a
doubleIt x = add x x

