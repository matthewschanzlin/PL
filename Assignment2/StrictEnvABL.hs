{- |
Module      :  StrictEnvABL
Description :  A strict, environment-based implementation of ABL.

Maintainer  :  Your Name <your email>
-}

module StrictEnvABL where

import Env
import ABL

import SimpleTests

import Debug.Trace (trace) -- for debugging purposes


-- Check application of binary operations to `ABLValue`s
type BinOp a = (a -> a -> a)

-- Apply an binary integer operation
applyIntegerBinOp :: BinOp Integer -> ABLValue -> ABLValue -> Maybe ABLValue
applyIntegerBinOp f (Num n1) (Num n2) = Just (Num (f n1 n2))
applyIntegerBinOp f _ _ = Nothing

-- Apply a binary boolean operation
applyBoolBinOp :: BinOp Bool -> ABLValue -> ABLValue -> Maybe ABLValue
applyBoolBinOp f (Bool b1) (Bool b2) = Just (Bool (f b1 b2))
applyBoolBinOp (==) (Num n1) (Num n2) = 
  case n1 - n2 of
    0 -> Just (Bool True)
    _ -> Just (Bool False)
applyBoolBinOp f _ _ = Nothing

-- Evaluate an ABL expression in the given environment
evalABL :: Env ABLValue -> ABLExpr -> Maybe ABLValue
evalABL env (Var x) = get x env
evalABL env (Val v) = Just v
evalABL env (Add e1 e2) = 
  case evalABL env e1 of
       Just v1 -> case evalABL env e2 of
                       Just v2 -> applyIntegerBinOp (+) v1 v2
                       Nothing -> Nothing
       Nothing -> Nothing
evalABL env (Sub e1 e2) = 
  case evalABL env e1 of
       Just v1 -> case evalABL env e2 of
                       Just v2 -> applyIntegerBinOp (-) v1 v2
                       Nothing -> Nothing
       Nothing -> Nothing
evalABL env (Mul e1 e2) = 
  case evalABL env e1 of
       Just v1 -> case evalABL env e2 of
                       Just v2 -> applyIntegerBinOp (*) v1 v2
                       Nothing -> Nothing
       Nothing -> Nothing
evalABL env (Div e1 e2) = 
  case evalABL env e1 of
       Just v1 -> case evalABL env e2 of
                       Just v2 -> applyIntegerBinOp div v1 v2
                       Nothing -> Nothing
       Nothing -> Nothing
evalABL env (And e1 e2) = 
  case evalABL env e1 of
       Just v1 -> case evalABL env e2 of
                       Just v2 -> applyBoolBinOp (&&) v1 v2
                       Nothing -> Nothing
       Nothing -> Nothing
evalABL env (Or e1 e2) = 
  case evalABL env e1 of
       Just v1 -> case evalABL env e2 of
                       Just v2 -> applyBoolBinOp (||) v1 v2
                       Nothing -> Nothing
       Nothing -> Nothing
evalABL env (Eq e1 e2) = 
  case evalABL env e1 of
       Just v1 -> case evalABL env e2 of
                       Just v2 -> applyBoolBinOp (==) v1 v2
                       Nothing -> Nothing
       Nothing -> Nothing
evalABL env (Not e1) = 
  case evalABL env e1 of
       Just v1 -> case applyBoolBinOp (==) v1 (Bool True) of
                      Just (Bool True) -> Just (Bool False)
                      Just (Bool False) -> Just (Bool True)
       Nothing -> Nothing
evalABL env (If e1 e2 e3) = 
  case evalABL env e1 of
       Just (Bool True) -> evalABL env e2
       Just (Bool False) -> evalABL env e3
       Nothing -> Nothing
evalABL env (Let1 e1 e2 e3) = 
  case evalABL env e2 of
       Just v1 -> case evalABL (add e1 v1 env) e3 of
                      Just v2 -> Just v2
                      Nothing -> Nothing
       Nothing -> Nothing


-- Check if the ABL expression is well-scoped, that is if all variables are 
-- defined before they are used.
--
-- The function is defined using a helper function scopeCheckAux. Your task is 
-- to complete the definition of scopeCheckAux.
scopeCheck :: ABLExpr -> Bool
scopeCheck e = scopeCheckAux [] e


-- Check if the given expression is well-scoped, provided the given variables
-- are visible. That is, scopeCheckAux vars (Var x) is well-scoped only if x 
-- appears in vars. For example, scopeCheck ["x"] (Var "x") is True, while 
-- scopeCheck ["x", "y"] (Var "z") is False.
scopeCheckAux :: [Variable] -> ABLExpr -> Bool
scopeCheckAux vars (Val _) = True
scopeCheckAux vars (Var x) = undefined 
  {- TASK: replace undefined with the definition -}
scopeCheckAux vars (Add e1 e2) = undefined
  {- TASK: complete the remaining equations -}

-- Helper function to express a series of bindings as a nested Let1 expression.
unfoldLetStar :: [(Variable, ABLExpr)] -> ABLExpr -> ABLExpr
unfoldLetStar [] e = undefined
unfoldLetStar ((x, ex) : bindings) e = undefined
{- TASK: replace `undefined` with the appropriate definitions -}


-- add your tests here
tests :: IO ()
tests = do
  test "eval empty 10" 
       (evalABL empty (Val (Num 10))) 
       (Just (Num 10))
  test "eval (+ 11 12)" 
       (evalABL empty (Add (Val (Num 11)) (Val (Num 12))))
       (Just (Num 23))
  test "eval (- 10 4)" 
       (evalABL empty (Sub (Val (Num 10)) (Val (Num 4))))
       (Just (Num 6))
  test "eval (* 5 3)" 
       (evalABL empty (Mul (Val (Num 5)) (Val (Num 3))))
       (Just (Num 15))
  test "eval (/ 64 16)" 
       (evalABL empty (Div (Val (Num 64)) (Val (Num 16))))
       (Just (Num 4))
  test "eval (&& True True)" 
       (evalABL empty (And (Val (Bool True)) (Val (Bool True))))
       (Just (Bool True))
  test "eval (&& True False)" 
       (evalABL empty (And (Val (Bool True)) (Val (Bool False))))
       (Just (Bool False))
  test "eval (not (&& True False))" 
       (evalABL empty (Not (And (Val (Bool True)) (Val (Bool False)))))
       (Just (Bool True))
  test "eval (not (&& True True))" 
       (evalABL empty (Not (And (Val (Bool True)) (Val (Bool True)))))
       (Just (Bool False))
  test "eval (if True (+ 2 10) (* 2 10))" 
       (evalABL empty (If (Val (Bool True)) (Add (Val (Num 2)) (Val (Num 10))) (Mul (Val (Num 2)) (Val (Num 10)))))
       (Just (Num 12))
  test "eval [(x, 3)] (if True (+ 2 10) (* 2 x))"
       (evalABL [("x", (Num 3))] (If (Val (Bool False)) (Add (Val (Num 2)) (Val (Num 10))) (Mul (Val (Num 2)) (Var "x"))))
       (Just (Num 6))
  test "eval (eq 5 4)"
       (evalABL [] (Eq (Val (Num 5)) (Val (Num 4))))
       (Just (Bool False))
  test "eval (eq 5 5)"
       (evalABL [] (Eq (Val (Num 5)) (Val (Num 5))))
       (Just (Bool True))
  test "eval (eq True False)"
       (evalABL [] (Eq (Val (Bool True)) (Val (Bool False))))
       (Just (Bool False))
  test "eval (eq True True)"
       (evalABL [] (Eq (Val (Bool True)) (Val (Bool True))))
       (Just (Bool True))
  test "eval (eq False False)"
       (evalABL [] (Eq (Val (Bool False)) (Val (Bool False))))
       (Just (Bool True))
  test ("eval (let x 4 (* x x))")
       (evalABL [] (Let1 "x" (Val (Num 4)) (Mul (Var "x") (Var "x"))))
       (Just (Num 16))
---------------------------- your helper functions --------------------------

