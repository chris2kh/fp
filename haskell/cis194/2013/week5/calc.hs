module Calc where
import ExprT
import Parser

eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

evalStr :: String -> Maybe Integer
evalStr x = 
    do
     expression <- parseExp Lit Add Mul x 
     let res = eval expression
     return res

evalStr' :: String -> Maybe Integer
evalStr' x =
   do
    let expression = parseExp Lit Add Mul x
    let res = fmap eval expression
    res


evalStr'' :: String -> Maybe Integer
evalStr''  = fmap eval . parseExp Lit Add Mul

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3* -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7


