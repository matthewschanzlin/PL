{- |
Module      :  StlcExt
Description :  Implementation of answers to Exercises 4-7.

Maintainer  :  Nicholas Seidl <seidl.n@husky.neu.edu>, Matthew Schanzlin <schanzlin.ma@husky.neu.edu>
-}

{-# OPTIONS_GHC -fdefer-typed-holes -fwarn-incomplete-patterns #-}
module StlcExt where

import Syntax
import Eval
import Types

import Maps

import SimpleTests

-- **Exercise 4**
-- first example: different types in the then branch and else branch of If
untypedButOk1 :: Expr
untypedButOk1 = (If (bool True) (num 4) (bool False))

-- second example: const comprised of multiple types
untypedButOk2 :: Expr
untypedButOk2 = (Cons (num 2) (Nil TyBool))

untypedButOk3 :: Expr
untypedButOk3 = _


-- For simplifying definitions of functions, you might find it worthwhile to
-- complete the following function. It should take a function name, a list of 
-- pairs of arguments names with their types, a return type and the body.
defineFun :: Variable -> [(Variable, Type)] -> Type -> Expr -> Expr
defineFun f args returnType body = 
  Fix (Lam f funType inner)
  where funType = (TyArrow (typeOf (typeOfArgs args) funType) -- complete
        inner = (Lam _ funType body)   -- complete

-- Example function definitions:

-- 1. Summing up the first n natural numbers
sumFirstN = 
  Fix (Lam "sum" (TyArrow TyInt TyInt) (Lam "n" TyInt (
    If (Leq (Var "n") (num 0))
       (num 0)
       (Add (Var "n") (App (Var "sum") (Sub (Var "n") (num 1)))))))

-- 2. Factorial
factorialExpr :: Expr
factorialExpr = Fix (
  Lam "f" (TyArrow TyInt TyInt) (
  Lam "x" TyInt (
    If (Leq (Var "x") (num 0))
       (num 1)
       (Mul (Var "x") (App (Var "f") (Sub (Var "x") (num 1))))
  )))

-- The same, using defineFun - if implemented
factorialExpr' :: Expr
factorialExpr' = 
  defineFun "f" [("x", TyInt)] TyInt (
    If (Leq (Var "x") (num 0))
       (num 1)
       (Mul (Var "x") (App (Var "f") (Sub (Var "x") (num 1))))
  )




-- Exercise 5
swapExpr :: Expr
swapExpr = _

swapExprType :: Type
swapExprType = _


-- Exercise 6
boolListLengthExpr :: Expr
boolListLengthExpr = _

boolListLengthExprType :: Type
boolListLengthExprType = _


-- Exercise 7
zipIntExpr :: Expr
zipIntExpr = _

zipIntExprType :: Type
zipIntExprType = _


---------------------------- your helper functions --------------------------
typeOfArgs :: [(Variable, Type)] -> Type
typeOfArgs (the_head:the_rest) = snd the_head

----------------------------------- TESTS -----------------------------------

tests :: IO ()
tests = do
  test "example typeOf test" 
    (typeOf empty sumFirstN) 
    (Just (TyArrow TyInt TyInt))
  test "example eval test" 
    (eval empty (App sumFirstN (num 5))) 
    (Just (Num 15))
  test "untypedButOk1 has no type" (typeOf empty untypedButOk1) Nothing
  test "untypedButOk2 has no type" (typeOf empty untypedButOk2) Nothing

