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

-- third example: a let expr where type of lam does not match type of the final expr
untypedButOk3 :: Expr
untypedButOk3 = (App (Lam "x" TyInt (num 4)) (bool True))


-- For simplifying definitions of functions, you might find it worthwhile to
-- complete the following function. It should take a function name, a list of 
-- pairs of arguments names with their types, a return type and the body.
defineFun :: Variable -> [(Variable, Type)] -> Type -> Expr -> Expr
defineFun f args returnType body = 
  Fix (Lam f funType inner)
  where funType = (TyArrow (typeOfArgs args) funType) -- complete
        inner = (lamSeries args body)   -- complete

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
swapExpr = 
  defineFun "f" [("x", (TyPair TyBool TyInt))] (TyPair TyInt TyBool) (
    Pair (Snd (Var "x")) (Fst (Var "x"))
  )

swapExprType :: Type
swapExprType = (TyPair TyInt TyBool)


---- Exercise 6
boolListLengthExpr :: Expr
boolListLengthExpr = 
  defineFun "f" [("x", (TyList TyBool))] TyInt (
    If (IsNil (Var "x"))
       (num 0)
       (Add (num 1) (App (Var "f") (Tail (Var "x"))))
  )

boolListLengthExprType :: Type
boolListLengthExprType = TyInt


---- Exercise 7
--zipIntExpr :: Expr
--zipIntExpr = _

--zipIntExprType :: Type
--zipIntExprType = _


---------------------------- your helper functions --------------------------
typeOfArgs :: [(Variable, Type)] -> Type
typeOfArgs [] = (TyList TyInt)
typeOfArgs (first_tup:rest) = (TyArrow (snd first_tup) (typeOfArgs rest))

lamSeries :: [(Variable, Type)] -> Expr -> Expr
lamSeries [] body = body
lamSeries (first_tup:rest) body = (Lam (fst first_tup) (snd first_tup) (lamSeries rest body))

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
  test "factorial == factorial'"
    ((eval empty (App factorialExpr' (num 4))) == (eval empty (App factorialExpr (num 4))))
    (True)
  test "swapExpr"
    (eval empty (App swapExpr (Pair (bool False) (num 3))))
    (Just (VPair (Num 3) (Bool False)))
  test "swapExpr type"
    (typeOf empty (Pair (num 3) (bool False)))
    (Just swapExprType)
  test "boolListLengthExpr"
    (eval empty (App boolListLengthExpr (Cons (bool True) (Cons (bool False) (Cons (bool False) (Nil TyBool))))))
    (Just (Num 3))
  test "boolListLengthExpr type"
    (typeOf empty (num 3))
    (Just boolListLengthExprType)