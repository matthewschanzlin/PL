{- |
Module      :  MiniImp
Description :  Implementation of the MiniImp language.

Maintainer  :  Nicholas Seidl <seidl.n@husky.neu.edu>, Matthew Schanzlin <schanzlin.ma@husky.neu.edu>
-}

module MiniImp where

import Store

import SimpleTests

import Debug.Trace (trace) -- useful for debugging purposes 

type Variable = String

data Value = Num Integer
           | Bool Bool
           | Array () -- replace with your representation of arrays
           deriving (Show, Eq)

data Expr = Val Value
          | Var String
          -- arithmetic expressions
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          -- boolean expressions
          | And Expr Expr
          | Not Expr
          -- less than or equal to
          | Le Expr Expr
          -- get the value stored in array at index
          | Get Variable Expr
          deriving (Show, Eq)

-- shorthands for values
num n = Val (Num n)
bool b = Val (Bool b)

data Stmt = Assign Variable Expr
          | Seq Stmt Stmt
          | While Expr Stmt
          | If Expr Stmt Stmt
          | Print Expr
          -- perform the body while the condition is true, but at least once
          | DoWhile Stmt Expr
          -- loop the specified number of times, using a counter variable
          | For Variable Expr Expr Stmt
          -- read a number from the input stream and store it in the given 
          -- variable
          | Read Variable
          -- allocate a new array
          | NewArray Variable Expr Expr
          -- set a value in array at index
          | Set Variable Expr Expr
          -- execute body for each 
          | ForEach Variable Variable Stmt
          -- for-loop with step specification
          deriving (Show, Eq)


type In = [Integer]
type Out = [Value]

-- expression evaluation
evalExpr :: Store Value -> Expr -> Maybe Value
evalExpr _ (Val v) = return v
evalExpr sto (Var x) = get x sto
evalExpr sto (Add e1 e2) = 
  do Num n1 <- evalExpr sto e1
     Num n2 <- evalExpr sto e2
     return (Num (n1 + n2))
evalExpr sto (Sub e1 e2) = 
  do Num n1 <- evalExpr sto e1
     Num n2 <- evalExpr sto e2
     return (Num (n1 - n2))
evalExpr sto (Mul e1 e2) = 
  do Num n1 <- evalExpr sto e1
     Num n2 <- evalExpr sto e2
     return (Num (n1 * n2))
evalExpr sto (And e1 e2) = 
  do Bool b1 <- evalExpr sto e1
     Bool b2 <- evalExpr sto e2
     return (Bool (b1 && b2))
evalExpr sto (Not e) = 
  do Bool b <- evalExpr sto e
     return (Bool (not b))
evalExpr sto (Le e1 e2) = 
  do Num n1 <- evalExpr sto e1
     Num n2 <- evalExpr sto e2
     return (Bool (n1 <= n2))
--evalExpr sto (Get v1 e1) = 
--  do 
-- complete the definition
evalExpr _ _ = error "Definition of evalExpr is incomplete!"

-- evaluation of statements
execStmt :: (Stmt, Store Value, In) -> Maybe (Store Value, In, Out)
execStmt (Assign x e, sto, i) =
  do v <- evalExpr sto e
     return (add x v sto, i, [])
execStmt (Seq s1 s2, sto, i) = 
  do (sto', i', out1) <- execStmt (s1, sto, i)
     (sto'', i'', out2) <- execStmt (s2, sto', i')
     return (sto'', i'', out1 ++ out2)
execStmt (Print expr1, sto1, in1) =
  do val1 <- evalExpr sto1 expr1
     return (sto1, in1, [val1])
execStmt (While expr1 stmt1, sto1, in1) =
  case evalExpr sto1 expr1 of
    Just (Bool False) -> Just (sto1, in1, [])
    Just (Bool True) -> case execStmt (stmt1, sto1, in1) of
      Just (sto2, in2, out2) -> case execStmt (While expr1 stmt1, sto2, in2) of
        Just (sto3, in3, out3) -> return (sto3, in3, out2 ++ out3)
execStmt (If expr1 st1 st2, sto1, in1) =
  case evalExpr sto1 expr1 of
    Just (Bool True) -> case execStmt (st1, sto1, in1) of
      Just (st1', in1', out1) -> return (st1', in1', out1)
    Just (Bool False) -> case execStmt (st2, sto1, in1) of
      Just (st2', in1', out2) -> return (st2', in1', out2)
execStmt (DoWhile stmt1 expr1, sto1, in1) =
  case execStmt (stmt1, sto1, in1) of
    Just (sto1', in1', out1) -> case evalExpr sto1 expr1 of
      Just (Bool False) -> Just (sto1', in1', out1)
      Just (Bool True) -> case execStmt (While expr1 stmt1, sto1', in1') of
        Just (sto2, in2, out2) -> return (sto2, in2, out1 ++ out2)
      
-- complete the definition
execStmt _ = error "Definition of execStmt is incomplete!"

exercise6 :: Stmt
exercise6 = undefined

exercise7 :: Stmt
exercise7 = undefined

---------------------------- your helper functions --------------------------


----------------------------------- TESTS -----------------------------------

-- Helpers for testing
-- Feel free to introduce further shorthands to help with testing
selectIn :: Maybe (Store a, In, Out) -> Maybe In
selectIn (Just (_, i, _)) = Just i
selectIn _ = Nothing

selectOut :: Maybe (Store a, In, Out) -> Maybe Out
selectOut (Just (_, _, o)) = Just o
selectOut _ = Nothing

cfgWithIn :: Stmt -> In -> (Stmt, Store Value, In)
cfgWithIn s i = (s, empty, i)

emptyCfg :: Stmt -> (Stmt, Store Value, In)
emptyCfg s = cfgWithIn s []

execToOutWithIn :: Stmt -> In -> Maybe Out
execToOutWithIn s i = selectOut (execStmt (cfgWithIn s i))

execToOut :: Stmt -> Maybe Out
execToOut s = execToOutWithIn s []

tests :: IO ()
tests = do
  -- example tests - add more
  test "print 10"
       (execStmt (Print (Val (Num 10)), empty, [])) 
       (Just (empty, [], [Num 10]))
  --test "read then print 42"
  --     (execToOutWithIn (Seq (Read "x") (Print (Var "x"))) [42])
  --     (Just [Num 42])
  --test "do { print 12 } while false"
  --     (execToOut (DoWhile (Print (num 12)) (bool False)))
  --     (Just [Num 12])
  --test "for x = 1 to 5 { print x }"
  --     (execToOut (For "x" (num 1) (num 5) (Print (Var "x"))))
  --     (Just [Num 1, Num 2, Num 3, Num 4, Num 5])
  --test "Array with 5 elements"
  --     (execToOut (NewArray "array" (num 5) (num 5) `Seq`
  --                 Set "array" (num 2) (num 42) `Seq`           
  --                 Print (Get "array" (num 0)) `Seq`
  --                 Print (Get "array" (num 1)) `Seq`
  --                 Print (Get "array" (num 2)) `Seq`
  --                 Print (Get "array" (num 3)) `Seq`
  --                 Print (Get "array" (num 4))))
  --     (Just [Num 5, Num 5, Num 42, Num 5, Num 5])
  --test "foreach print 0 1 2 3"
  --     (execToOut (NewArray "array" (num 4) (num 0) `Seq`
  --                 Set "array" (num 1) (num 1) `Seq`
  --                 Set "array" (num 2) (num 2) `Seq`
  --                 Set "array" (num 3) (num 3) `Seq`
  --                 ForEach "x" "array" (Print (Var "x"))))
  --     (Just [Num 0, Num 1, Num 2, Num 3])
 
