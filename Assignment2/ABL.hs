{- |
Module      :  ABL
Description :  Syntax of the ABL language.

Maintainer  :  Your Name <your email>
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
             {- TASK: complete the remaining constructors -}
             deriving (Show, Eq)

showABL :: ABLExpr -> String
showABL (Var x) = x
showABL (Val (Num n)) = show n
showABL (Val (Bool b)) = 
  if b 
     then "true" 
     else "false"
showABL (Add e1 e2) = "(+ " ++ showABL e1 ++ " " ++ showABL e2 ++ ")"
{- TASK: complete the remaining cases -}


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

