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
             | Let1 ABLExpr ABLExpr ABLExpr
             | If ABLExpr ABLExpr ABLExpr
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
showABL (Let1 v1 e1 e2) = "(let1 (" ++ showABL v1 ++ " " ++ showABL e1 ++ ") " ++ showABL e2 ++ ")"
showABL (If e1 e2 e3) = "(if-else " ++ showABL e1 ++ " " ++ showABL e2 ++ " " ++ showABL e3 ++ ")"


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

---------------------------- your helper functions --------------------------

