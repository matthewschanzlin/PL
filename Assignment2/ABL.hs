{- |
Module      :  ABL
Description :  Syntax of the ABL language.

Maintainer  :  Nicholas Seidl <seidl.n@husky.neu.edu>, Matthew Schanzlin <Matthew Schanzlin <schanzlin.ma@husky.neu.edu>
-}

module ABL where

import SimpleTests

-- We represent variables as strings and introduce `Variable` as an alias for
-- `String`.
type Variable = String

-- ABL values are either numbers (integers) or booleans.
data ABLValue = Num Integer
              | Bool Bool
              deriving (Show, Eq)

data ABLExpr = Var Variable
             | Val ABLValue
             | Add ABLExpr ABLExpr
             | Sub ABLExpr ABLExpr
             | Mul ABLExpr ABLExpr
             | Div ABLExpr ABLExpr
             | Eq ABLExpr ABLExpr
             | And ABLExpr ABLExpr
             | Or ABLExpr ABLExpr
             | Not ABLExpr
             | Let1 Variable ABLExpr ABLExpr
             | If ABLExpr ABLExpr ABLExpr
             | Fresh ABLExpr
             | LetStar [(Variable, ABLExpr)] ABLExpr
             deriving (Show, Eq)

showABL :: ABLExpr -> String
showABL (Var x) = x
showABL (Val (Num n)) = show n
showABL (Val (Bool b)) =
  if b
     then "true"
     else "false"
showABL (Add e1 e2) = "(+ " ++ showABL e1 ++ " " ++ showABL e2 ++ ")"
showABL (Sub e1 e2) = "(- " ++ showABL e1 ++ " " ++ showABL e2 ++ ")"
showABL (Mul e1 e2) = "(* " ++ showABL e1 ++ " " ++ showABL e2 ++ ")"
showABL (Div e1 e2) = "(/ " ++ showABL e1 ++ " " ++ showABL e2 ++ ")"
showABL (Eq e1 e2) = "(= " ++ showABL e1 ++ " " ++ showABL e2 ++ ")"
showABL (And e1 e2) = "(and " ++ showABL e1 ++ " " ++ showABL e2 ++ ")"
showABL (Or e1 e2) = "(or " ++ showABL e1 ++ " " ++ showABL e2 ++ ")"
showABL (Not e1) = "(not " ++ showABL e1 ++ ")"
showABL (Let1 v1 e1 e2) = "(let1 (" ++ showABL (Var v1) ++ " " ++ showABL e1 ++ ") " ++ showABL e2 ++ ")"
showABL (If e1 e2 e3) = "(if-else " ++ showABL e1 ++ " " ++ showABL e2 ++ " " ++ showABL e3 ++ ")"
showABL (Fresh e1) = "(fresh-env " ++ showABL e1 ++ ")"
showABL (LetStar l e) = "(let* (" ++ showABLHelper l ++ ") " ++ showABL e ++ ")"

showABLHelper :: [(Variable, ABLExpr)] -> String
showABLHelper [] = ""
showABLHelper ((v, e) : []) = "(" ++ showABL (Var v) ++ " " ++ showABL e ++ ")"
showABLHelper ((v, e) : rest) = "(" ++ showABL (Var v) ++ " " ++ showABL e ++ ") " ++ showABLHelper rest

-- add tests
tests :: IO ()
tests = do
  test "showABL num"
       (showABL (Val (Num 10)))
       "10"
  test "showABL true"
       (showABL (Val (Bool True)))
       "true"
  test "showABL false"
       (showABL (Val (Bool False)))
       "false"
  test "showABL let"
       (showABL (Let1 "x" (Val (Num 5)) (Mul (Var "x") (Val (Num 10)))))
       "(let1 (x 5) (* x 10))"
  test "showABL let*"
       (showABL (LetStar [("x", (Val (Num 5))), ("y", (Val (Num 6)))] (Mul (Var "x") (Var "y"))))
       "(let* ((x 5) (y 6)) (* x y))"

---------------------------- your helper functions --------------------------
