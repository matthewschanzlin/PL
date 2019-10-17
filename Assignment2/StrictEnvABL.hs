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
evalABL env (Let1 v1 e1 e2) =
  case evalABL env e1 of
       Just v -> case evalABL (add v1 v env) e2 of
                      Just v2 -> Just v2
                      Nothing -> Nothing
       Nothing -> Nothing
evalABL env (Fresh e1) = evalABL empty e1
evalABL env (LetStar l1 e) = evalABL env (unfoldLetStar l1 e)


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
scopeCheckAux vars (Var x) = elem x vars
scopeCheckAux vars (Add e1 e2) = scopeCheckAux vars e1 && scopeCheckAux vars e2
scopeCheckAux vars (Sub e1 e2) = scopeCheckAux vars e1 && scopeCheckAux vars e2
scopeCheckAux vars (Mul e1 e2) = scopeCheckAux vars e1 && scopeCheckAux vars e2
scopeCheckAux vars (Div e1 e2) = scopeCheckAux vars e1 && scopeCheckAux vars e2
scopeCheckAux vars (Eq e1 e2) = scopeCheckAux vars e1 && scopeCheckAux vars e2
scopeCheckAux vars (And e1 e2) = scopeCheckAux vars e1 && scopeCheckAux vars e2
scopeCheckAux vars (Or e1 e2) = scopeCheckAux vars e1 && scopeCheckAux vars e2
scopeCheckAux vars (Not e1) = scopeCheckAux vars e1
scopeCheckAux vars (If e1 e2 e3) =
  case scopeCheckAux vars e1 of
    True -> case evalABL [] e1 of
      Just (Bool True) -> scopeCheckAux vars e2
      Just (Bool False) -> scopeCheckAux vars e3
    False -> False
scopeCheckAux vars (Let1 v1 e1 e2) = scopeCheckAux vars e1 && scopeCheckAux (v1 : vars) e2
scopeCheckAux vars (Fresh e1) = scopeCheckAux vars e1
scopeCheckAux vars (LetStar l e)
  | l == empty = scopeCheckAux vars e
  | otherwise = scopeCheckAux vars (Var (fst (head l))) && scopeCheckAux vars (LetStar (tail l) e)

-- Helper function to express a series of bindings as a nested Let1 expression.
unfoldLetStar :: [(Variable, ABLExpr)] -> ABLExpr -> ABLExpr
unfoldLetStar [] e = e
unfoldLetStar ((x, ex) : bindings) e = (Let1 x ex (unfoldLetStar bindings e))

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
  test "eval (not (|| False False))"
       (evalABL empty (Not (Or (Val (Bool False)) (Val (Bool False)))))
       (Just (Bool True))
  test "eval (not (|| True True))"
       (evalABL empty (Not (Or (Val (Bool True)) (Val (Bool True)))))
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
  test ("eval (let (x 4) (* x x))")
       (evalABL [] (Let1 "x" (Val (Num 4)) (Mul (Var "x") (Var "x"))))
       (Just (Num 16))
  test ("eval (let* ((x 4) (y 5)) (* x y))")
       (evalABL [] (LetStar [("x", (Val (Num 4))),("y", (Val (Num 5)))] (Mul (Var "x") (Var "y"))))
       (Just (Num 20))
  test ("eval (fresh-env env)")
       (evalABL [("x", (Num 6)),("x", (Num 7))] (Fresh (Let1 "y" (Val (Num 3)) (Add (Var "x") (Var "y")))))
       Nothing

  test ("scopeCheck val")
       (scopeCheckAux [] (Val (Num 5)))
       True
  test ("scopeCheck var")
       (scopeCheckAux [] (Var "x"))
       False
  test ("scopeCheck varY")
       (scopeCheckAux ["y"] (Var "x"))
       False
  test ("scopeCheck varX")
       (scopeCheckAux ["x"] (Var "x"))
       True
  test ("scopeCheck add")
       (scopeCheckAux [] (Add (Val (Num 4)) (Val (Num 5))))
       True
  test ("scopeCheck addx")
       (scopeCheckAux [] (Add (Val (Num 4)) (Var "x")))
       False
  test ("scopeCheck addx")
       (scopeCheckAux ["x"] (Add (Val (Num 4)) (Var "x")))
       True
  test ("scopeCheck addx")
       (scopeCheckAux ["x"] (Add (Val (Num 4)) (Var "x")))
       True
  test ("scopeCheck addx")
       (scopeCheckAux ["x"] (Add (Val (Num 4)) (Var "y")))
       False
  test ("scopeCheck sub")
       (scopeCheckAux ["x"] (Sub (Var "y") (Val (Num 4))))
       False
  test ("scopeCheck sub")
       (scopeCheckAux ["x"] (Sub (Var "x") (Val (Num 4))))
       True
  test ("scopeCheck mul")
       (scopeCheckAux ["x"] (Mul (Val (Num 4)) (Var "x")))
       True
  test ("scopeCheck div")
       (scopeCheckAux ["x"] (Div (Val (Num 4)) (Var "y")))
       False
  test ("scopeCheck and")
       (scopeCheckAux ["x"] (And (Var "y") (Val (Num 4))))
       False
  test ("scopeCheck or")
       (scopeCheckAux ["x"] (Or (Var "x") (Val (Num 4))))
       True
  test ("scopeCheck not")
       (scopeCheckAux ["x"] (Not (Var "y")))
       False
  test ("scopeCheck eq")
       (scopeCheckAux ["x"] (Eq (Var "x") (Val (Num 4))))
       True
  test ("scopeCheck if")
       (scopeCheckAux ["x"] (If (Val (Bool False)) (Val (Num 4)) (Var "y")))
       False
  test ("scopeCheck if")
       (scopeCheckAux ["y", "x"] (If (Val (Bool False)) (Val (Num 4)) (Var "x")))
       True
  test ("scopeCheck if")
       (scopeCheckAux ["x"] (If (Val (Bool True)) (Val (Num 4)) (Var "y")))
       True
  test ("scopeCheck let1")
       (scopeCheckAux ["x"] (Let1 "y" (Val (Num 4)) (Mul (Var "x") (Var "x"))))
       True
  test ("scopeCheck let1")
       (scopeCheckAux ["x"] (Let1 "y" (Val (Num 4)) (Mul (Var "y") (Var "x"))))
       True
  test ("scopeCheck let1")
       (scopeCheckAux ["x"] (Let1 "y" (Val (Num 4)) (Mul (Var "z") (Var "x"))))
       False
  test ("scopeCheck fresh")
       (scopeCheckAux ["x"] (Fresh (Var "x")))
       True
  test ("scopeCheck fresh")
       (scopeCheckAux ["y"] (Fresh (Var "x")))
       False
  test ("scopeCheck let*")
       (scopeCheckAux ["x","y","z"] (LetStar [] (Mul (Val (Num 45)) (Val (Num 56)))))
       True
  test ("scopeCheck let*")
       (scopeCheckAux ["x","y","z"] (LetStar [] (Mul (Var "x") (Val (Num 56)))))
       True
  test ("scopeCheck let*")
       (scopeCheckAux ["x","y","z"] (LetStar [] (Mul (Var "r") (Val (Num 56)))))
       False
  test ("scopeCheck let*")
       (scopeCheckAux ["x","y","z"] (LetStar [("x", (Val (Num 5))),("y", (Val (Num 7))),("z", (Val (Num 4)))] (Mul (Var "x") (Val (Num 56)))))
       True
  test ("scopeCheck let*")
       (scopeCheckAux ["x","y","z"] (LetStar [("x", (Val (Num 5))),("r", (Val (Num 7))),("z", (Val (Num 4)))] (Mul (Var "x") (Val (Num 56)))))
       False
---------------------------- your helper functions --------------------------
