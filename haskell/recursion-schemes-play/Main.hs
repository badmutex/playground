{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Functor.Foldable


data ExprF a
  = Literal Int
  | Add a a
  | Call a [a]
  deriving (Show, Functor)

type Expr = Fix ExprF


ten, five, call :: Expr
ten = Fix (Literal 10)
five = Fix (Literal 5)
call = Fix (Add ten five)

countLiterals :: ExprF Int -> Int
countLiterals (Literal a) = 1
countLiterals (Add lhs rhs) = lhs + rhs
countLiterals (Call a as) = a + sum as

test1 = 2 == cata countLiterals call

        ----------------------------------------------------------------- 

data Expr2F a
  = Lit Int
  | Bind String a
  | BinOp a Char a
  deriving (Show, Functor)

type Expr2 = Fix Expr2F

forty, two, three, fortythree, bind, op :: Expr2
forty = Fix $ Lit 40
two = Fix $ Lit 2
three = Fix $ Lit 3
fortythree = Fix $ BinOp forty '+' three
bind = Fix $ Bind "two" two
op = Fix $ BinOp bind '+' forty

evalExpr2 :: Expr2F Int -> Int
evalExpr2 (Lit e) = e
evalExpr2 (Bind _ e) = e
evalExpr2 (BinOp lhs op rhs) =
  case op of
    '+' -> lhs + rhs
    other -> error $ "Unknown BinOp " ++ [other]

evalBinOp :: Char -> Int -> Int -> Int
evalBinOp = \case
  '+' -> (+)
  '-' -> (-)
  '*' -> (*)
  op -> error $ "Unsupported binop " ++ [op]

evalExpr2Even :: Expr2F (Maybe Int) -> Maybe Int
evalExpr2Even (Lit m) = pure m
evalExpr2Even (Bind _ e) = e
evalExpr2Even (BinOp lhs op rhs) = do
  lhs' <- lhs
  rhs' <- rhs
  if (even lhs') && (even rhs')
    then pure $ evalBinOp op lhs' rhs'
    else Nothing

test2 = 42 == cata evalExpr2 op


        -----------------------------------------------------------------




main :: IO ()
main = putStrLn "Hello, Haskell!"
  
