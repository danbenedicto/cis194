{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

module Calc where

import ExprT
import Parser
import qualified StackVM
import qualified Data.Map as M

eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add x y) = (eval x) + (eval y)
eval (Mul x y) = (eval x) * (eval y)

evalStr :: String -> Maybe Integer
evalStr s = let p = parseExp Lit Add Mul s
            in case p of
              (Just expr) -> Just (eval expr)
              Nothing -> Nothing

class Expr a where
  lit      :: Integer -> a
  add, mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (>0)
  add = (||)
  mul = (&&)

newtype MinMax  = MinMax Integer deriving (Eq, Show)
newtype Mod7    = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax
  add (MinMax a) (MinMax b) = MinMax (max a b)
  mul (MinMax a) (MinMax b) = MinMax (min a b)

instance Expr Mod7 where
  lit                   = Mod7 . (`mod` 7)
  add (Mod7 a) (Mod7 b) = Mod7 $ (a + b) `mod` 7
  mul (Mod7 a) (Mod7 b) = Mod7 $ (a * b) `mod` 7

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3*-4) + 5"

-- testInteger  = testExp :: Maybe Integer
-- testBool     = testExp :: Maybe Bool
-- testMM       = testExp :: Maybe MinMax
-- testSat      = testExp :: Maybe Mod7

instance Expr StackVM.Program where
  lit n   = [StackVM.PushI n]
  add a b = a ++ b ++ [StackVM.Add]
  mul a b = a ++ b ++ [StackVM.Mul]

compile :: String -> Maybe StackVM.Program
compile s = parseExp lit add mul s

-- testProg = case compile "(3*-4) + 5" of
--   (Just prog) -> StackVM.stackVM prog
--   _ -> Left "fail"

-- Exercise 6
class HasVars a where
  var :: String -> a

data VarExprT = Lit' Integer
              | Add' VarExprT VarExprT
              | Mul' VarExprT VarExprT
              | Var' String
  deriving (Show, Eq)

instance Expr VarExprT where
  lit = Lit'
  add = Add'
  mul = Mul'

instance HasVars VarExprT where
  var = Var'

type MapExpr = M.Map String Integer -> Maybe Integer

instance HasVars MapExpr where
  var s = (M.!?s)

instance Expr MapExpr where
  lit = const . Just
  add = mapOp (+)
  mul = mapOp (*)

mapOp :: (Integer -> Integer -> Integer) -> MapExpr -> MapExpr -> MapExpr
mapOp op fa fb m =
  let a = (fa m)
      b = (fb m)
  in case (a, b) of
    (Just n1, Just n2) -> Just (n1 `op` n2)
    _ -> Nothing

withVars :: [(String, Integer)] -> (M.Map String Integer -> Maybe Integer) -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
