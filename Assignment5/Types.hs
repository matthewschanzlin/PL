{- |
Module      :  Types
Description :  Type-checker implementation.

Maintainer  :  Ferd <f.vesely@northeastern.edu>
               Your Name <your email>
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
     aType <- typeOf tenv e2
     return aType
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
     return (TyList type1)
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
  test "|- 4 + 5 : TyInt" (typeOf empty (Add (num 4) (num 5))) (Just TyInt)
  test "|- (Pair 4 True): TyPair TyInt TyBool" (typeOf empty (Pair (Val (Num 4)) (Val (Bool True)))) (Just (TyPair TyInt TyBool))
  test "|- Snd (Pair 4 True): TyBool" (typeOf empty (Snd (Pair (Val (Num 4)) (Val (Bool True))))) (Just TyBool)
  test "|- Fst (Pair 4 True): TyBool" (typeOf empty (Fst (Pair (Val (Num 4)) (Val (Bool True))))) (Just TyInt)