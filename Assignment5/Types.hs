{- |
Module      :  Types
Description :  Type-checker implementation.

Maintainer  :  Ferd <f.vesely@northeastern.edu>
               Maintainer  :  Nicholas Seidl <seidl.n@husky.neu.edu>, Matthew Schanzlin <schanzlin.ma@husky.neu.edu>
-}

{-# OPTIONS_GHC -fdefer-typed-holes -fwarn-incomplete-patterns #-}
module Types where

import Syntax

import Maps

import Debug.Trace (trace)

import SimpleTests


-- complete the definition below
typeOf :: TEnv -> Expr -> Maybe Type
typeOf tenv (Val (Bool _)) = return TyBool
typeOf tenv (Val (Num _)) = return TyInt
typeOf tenv e@(Val _) = fail ("Runtime-only value " ++ show e)
typeOf tenv (Var x) = get x tenv
typeOf tenv (Lam x t1 e) =
  do t2 <- typeOf (add x t1 tenv) e
     return (TyArrow t1 t2)
typeOf tenv e@(App e1 e2) =
  do TyArrow t2 t1 <- typeOf tenv e1
     t2' <- typeOf tenv e2
     if t2 == t2'
        then return t1
        else fail "App"
typeOf tenv (Fix e) =
  do TyArrow t1 t2 <- typeOf tenv e
     if t1 == t2
        then return t1
        else fail "Fix"
typeOf tenv (Let x e1 e2) =
  do
    t1 <- typeOf tenv e1
    t2 <- typeOf (add x t1 tenv) e2
    return t2
typeOf tenv (Add e1 e2) =
  do TyInt <- typeOf tenv e1
     TyInt <- typeOf tenv e2
     return TyInt
typeOf tenv (Sub e1 e2) =
  do TyInt <- typeOf tenv e1
     TyInt <- typeOf tenv e2
     return TyInt
typeOf tenv (Mul e1 e2) =
  do TyInt <- typeOf tenv e1
     TyInt <- typeOf tenv e2
     return TyInt
typeOf tenv (And e1 e2) =
  do TyBool <- typeOf tenv e1
     TyBool <- typeOf tenv e2
     return TyBool
typeOf tenv (Not e1) =
  do TyBool <- typeOf tenv e1
     return TyBool
typeOf tenv (Leq e1 e2) =
  do TyInt <- typeOf tenv e1
     TyInt <- typeOf tenv e2
     return TyBool
typeOf tenv (If e1 e2 e3) =
  do TyBool <- typeOf tenv e1
     aType1 <- typeOf tenv e2
     aType2 <- typeOf tenv e3
     case aType1 == aType2 of
       True -> return aType1
       False -> Nothing
typeOf tenv (Pair e1 e2) =
  do type1 <- typeOf tenv e1
     type2 <- typeOf tenv e2
     return (TyPair type1 type2)
typeOf tenv (Fst e1) =
  do (TyPair type1 type2) <- typeOf tenv e1
     return type1
typeOf tenv (Snd e1) =
  do (TyPair type1 type2) <- typeOf tenv e1
     return type2
typeOf tenv (Cons e1 e2) =
  do type1 <- typeOf tenv e1
     (TyList type2) <- typeOf tenv e2
     case type1 == type2 of
       True -> return (TyList type1)
       False -> Nothing
typeOf tenv (Nil type1) =
  return (TyList type1)
typeOf tenv (IsNil e1) =
  return TyBool
typeOf tenv (Head e1) =
  do (TyList type1) <- typeOf tenv e1
     return type1
typeOf tenv (Tail e1) =
  do (TyList type1) <- typeOf tenv e1
     return (TyList type1)


---------------------------- your helper functions --------------------------


----------------------------------- TESTS -----------------------------------

tests :: IO ()
tests = do
  test "|- 4 : TyInt" (typeOf empty (Val (Num 4))) (Just TyInt)
  test "|- True : TyBool" (typeOf empty (Val (Bool True))) (Just TyBool)
  test "|- x : " (typeOf empty (Var "x")) Nothing
  test "|- x : " (typeOf [("x", TyInt)] (Var "x")) (Just TyInt)
  test "|- (Lam x t1 e): (Just (TyArrow t1 (typeOf (Add x t1 tenv) e))" (typeOf [("x", TyInt)] (Lam "x" TyInt (Val (Num 4)))) (Just (TyArrow TyInt TyInt))
  test "|- (App e1 e2): Nothing" (typeOf empty (App (Lam "x" TyInt (Val (Num 4))) (Var "x"))) Nothing
  test "|- (App e1 e2): TyInt" (typeOf [("x", TyInt)] (App (Lam "x" TyInt (Val (Num 4))) (Var "x"))) (Just TyInt)
  test "|- (Fix (App e1 e2))" (typeOf [("x", TyInt)] (Fix (Lam "x" TyInt (Val (Num 4))))) (Just TyInt)
  test "|- (Fix (App e1 e2))" (typeOf empty (Fix (Lam "x" TyInt (Val (Bool True))))) Nothing
  test "|- (Let x e1 e2)" (typeOf empty (Let "x" (Val (Num 4)) (Add (Var "x") (Var "x")))) (Just TyInt)
  test "|- (Add 3 5)" (typeOf empty (Add (Val (Num 3)) (Val (Num 3)))) (Just TyInt)
  test "|- (Add 3 5)" (typeOf empty (Add (Val (Num 3)) (Val (Bool True)))) Nothing
  test "|- (Sub 3 5)" (typeOf empty (Sub (Val (Num 3)) (Val (Num 3)))) (Just TyInt)
  test "|- (Sub 3 5)" (typeOf empty (Sub (Val (Num 3)) (Val (Bool True)))) Nothing
  test "|- (Mul 3 5)" (typeOf empty (Mul (Val (Num 3)) (Val (Num 3)))) (Just TyInt)
  test "|- (Mul 3 5)" (typeOf empty (Mul (Val (Num 3)) (Val (Bool True)))) Nothing
  test "|- (And 3 5)" (typeOf empty (Not (Val (Bool True)))) (Just TyBool)
  test "|- (And 3 5)" (typeOf empty (Not (Val (Num 3)))) Nothing
  test "|- (Leq 3 5)" (typeOf empty (Leq (Val (Num 3)) (Val (Num 3)))) (Just TyBool)
  test "|- (Leq 3 5)" (typeOf empty (Leq (Val (Num 3)) (Val (Bool True)))) Nothing
  test "|- (If 3 5)" (typeOf empty (If (Val (Bool True)) (Val (Bool True)) (Val (Bool True)))) (Just TyBool)
  test "|- (If 3 5)" (typeOf empty (If (Val (Num 3)) (Val (Bool True))(Val (Bool True)))) Nothing
  test "|- (If 3 5)" (typeOf empty (If (Val (Bool True)) (Val (Num 3)) (Val (Bool True)))) Nothing
  test "|- (And 3 5)" (typeOf empty (And (Val (Num 3)) (Val (Bool True)))) Nothing
  test "|- 4 + 5 : TyInt" (typeOf empty (Add (num 4) (num 5))) (Just TyInt)
  test "|- 4 + true : Nothing" (typeOf empty (Add (num 4) (bool True))) Nothing
  test "|- (Pair 4 True): TyPair TyInt TyBool" (typeOf empty (Pair (Val (Num 4)) (Val (Bool True)))) (Just (TyPair TyInt TyBool))
  test "|- Snd (Pair 4 True): TyBool" (typeOf empty (Snd (Pair (Val (Num 4)) (Val (Bool True))))) (Just TyBool)
  test "|- Fst (Pair 4 True): TyBool" (typeOf empty (Fst (Pair (Val (Num 4)) (Val (Bool True))))) (Just TyInt)
  test "|- Cons" (typeOf empty (Cons (Val (Num 3)) (Val (Num 3)))) Nothing
  test "|- Cons" (typeOf empty (Cons (Val (Num 3)) (Nil TyBool))) Nothing
  test "|- Cons" (typeOf empty (Cons (Val (Num 3)) (Nil TyInt))) (Just (TyList TyInt))
  test "|- Nil" (typeOf empty (Nil TyInt)) (Just (TyList TyInt))
  test "|- Head" (typeOf empty (Head (Cons (Val (Num 3)) (Nil TyInt)))) (Just TyInt)
  test "|- Head" (typeOf empty (Tail (Cons (Val (Num 3)) (Nil TyInt)))) (Just (TyList TyInt))
