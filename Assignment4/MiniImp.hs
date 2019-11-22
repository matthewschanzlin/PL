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
           | Array [Value] -- replace with your representation of arrays
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
evalExpr sto (Get var1 expr1) = 
  do 
    index <- evalExpr sto expr1
    array <- evalExpr sto (Var var1)
    case getItem array index of
      Just v -> return v

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
        _ -> Nothing
      _ -> Nothing
    _ -> Nothing
execStmt (If expr1 st1 st2, sto1, in1) =
  case evalExpr sto1 expr1 of
    Just (Bool True) -> case execStmt (st1, sto1, in1) of
      Just (st1', in1', out1) -> return (st1', in1', out1)
      _ -> Nothing
    Just (Bool False) -> case execStmt (st2, sto1, in1) of
      Just (st2', in1', out2) -> return (st2', in1', out2)
      _ -> Nothing
    _ -> Nothing
execStmt (DoWhile stmt1 expr1, sto1, in1) =
  case execStmt (stmt1, sto1, in1) of
    Just (sto1', in1', out1) -> case evalExpr sto1 expr1 of
      Just (Bool False) -> Just (sto1', in1', out1)
      Just (Bool True) -> case execStmt (While expr1 stmt1, sto1', in1') of
        Just (sto2, in2, out2) -> return (sto2, in2, out1 ++ out2)
        _ -> Nothing
      _ -> Nothing
    _ -> Nothing
execStmt (Read var1, sto1, in1) = 
  do 
    v <- execStmt (Assign var1 (Val (Num (head in1))), sto1, (tail in1))
    return v
execStmt (For var1 expr1 expr2 stmt1, sto1, in1) = 
  do 
    v1 <- evalExpr sto1 expr1
    (sto1', _, _) <- execStmt (Assign var1 (Val v1), sto1, in1)
    v <- execForHelper (For var1 expr1 expr2 stmt1, sto1', in1)
    return v
execStmt (NewArray var1 expr1 expr2, sto1, in1) =
  do
    size <- evalExpr sto1 expr1
    val <- evalExpr sto1 expr2
    case execStmt (Assign var1 (Val (createArray size val)), sto1, in1) of
      Just (sto1', in1', out1') -> return (sto1', in1', out1')
      _ -> Nothing
execStmt (Set var1 expr1 expr2, sto1, in1) = 
  do
    index <- evalExpr sto1 expr1
    newVal <- evalExpr sto1 expr2
    array <- evalExpr sto1 (Var var1)
    case replaceItem array index newVal of
      Just newVal -> case execStmt (Assign var1 (Val newVal), sto1, in1) of
        Just (sto1', in1', out1') -> return (sto1', in1', out1')
        _ -> Nothing
      _ -> Nothing
execStmt (ForEach var1 var2 stmt1, sto1, in1) = 
  do
    array <- evalExpr sto1 (Var var2)
    case getItem array (Num 0) of
      Just arrayVal -> do
        (sto1', in1', out1) <- execStmt (Assign var1 (Val arrayVal), sto1, in1)
        case execStmt (stmt1, sto1', in1') of
          Just (sto2, in2, out2) -> do
            case restOfArray array of
              Just restArray -> do
                (sto2', in2', out2') <- execStmt (Assign var2 (Val restArray), sto2, in2)
                case execStmt (ForEach var1 var2 stmt1, sto2', in2') of
                  Just (sto3, in3, out3) -> return (sto3, in3, out1 ++ out2 ++ out2' ++ out3)
                  _ -> Nothing
              _ -> Nothing
          _ -> Nothing
      _ -> return (sto1, in1, [])

exercise6 :: Stmt
exercise6 = Seq readAndMul printArray

readAndMul :: Stmt
readAndMul = Seq readValsToArray mulArray

printArray :: Stmt
printArray = (Seq (printEntry 0)
                (Seq (printEntry 1)
                  (Seq (printEntry 2)
                    (Seq (printEntry 3) (printEntry 4)))))

printEntry :: Integer -> Stmt
printEntry n = (Print (Get "array" (num n)))

mulArray :: Stmt
mulArray = (Seq (mulEntry 0)
              (Seq (mulEntry 1)
                (Seq (mulEntry 2)
                  (Seq (mulEntry 3) (mulEntry 4)))))

mulEntry :: Integer -> Stmt
mulEntry n = (Set "array" (num n) (Mul (Var "multiplier") (Get "array" (num n))))

readValsToArray :: Stmt
readValsToArray = Seq (Seq setupAndInput inputToArray) (Read "multiplier")

inputToArray :: Stmt
inputToArray = (Seq (Set "array" (num 0) (Var "0"))
                  (Seq (Set "array" (num 1) (Var "1"))
                    (Seq (Set "array" (num 2) (Var "2"))
                      (Seq (Set "array" (num 3) (Var "3"))
                           (Set "array" (num 4) (Var "4"))))))

setupAndInput :: Stmt
setupAndInput = Seq arr5to0 read5

read5 :: Stmt
read5 = (Seq (Read "0")
          (Seq (Read "1")
            (Seq (Read "2")
              (Seq (Read "3") (Read "4")))))

arr5to0 :: Stmt
arr5to0 = (NewArray "array" (num 5) (num 0))

exercise7 :: Stmt
exercise7 = undefined

---------------------------- your helper functions --------------------------
-- execForHelper assumes that var1 is inside the store
execForHelper :: (Stmt, Store Value, In) -> Maybe (Store Value, In, Out)
execForHelper (For var1 expr1 expr2 stmt1, sto1, in1) =
  do
    v1 <- evalExpr sto1 (Var var1)
    v2 <- evalExpr sto1 expr2
    case evalExpr sto1 (Le (Var var1) (Val v2)) of
      Just (Bool True) -> case execStmt (stmt1, sto1, in1) of
        Just (sto2, in2, out2) -> do
          v3 <- evalExpr sto2 (Add (Val v1) (Val (Num 1)))
          (sto2', _, _) <- execStmt (Assign var1 (Val v3), sto2, in2)
          case execForHelper (For var1 expr1 expr2 stmt1, sto2', in2) of
            Just (sto3, in3, out3) -> return (sto3, in3, out2 ++ out3)
      Just (Bool False) -> return (sto1, in1, [])

createArray :: Value -> Value -> Value
createArray (Num 0) _ = (Array [])
createArray (Num numVals) val = appendArray (Array [val]) (createArray (Num (numVals - 1)) val)

appendArray :: Value -> Value -> Value
appendArray (Array a) (Array b) = (Array (appendArrayHelper a b))

appendArrayHelper :: [Value] -> [Value] -> [Value]
appendArrayHelper a b = a ++ b

replaceItem :: Value -> Value -> Value -> Maybe Value
replaceItem (Array array) (Num index) val
  | fromInteger index > length array - 1  = Nothing
  | otherwise = Just (Array (replaceItemHelper array index val))

replaceItemHelper :: [Value] -> Integer -> Value -> [Value]
replaceItemHelper [] _ _ = []
replaceItemHelper (first_elem:other_elems) index newVal
  | index == 0 = newVal : other_elems
  | otherwise = first_elem : replaceItemHelper other_elems (index - 1) newVal

getItem :: Value -> Value -> Maybe Value
getItem (Array array) (Num index)
  | fromInteger index > length array - 1 = Nothing
  | otherwise = Just (array!!fromInteger index)

restOfArray :: Value -> Maybe Value
restOfArray (Array array) = case length array of
  0 -> Nothing
  1 -> Just (Array [])
  _ -> Just (Array (tail array))
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
  test "assign 10 x get x"
        (execToOut (Seq (Assign "x" (num 42)) (Print (Var "x"))))
        (Just [Num 42])
  test "assign True x get x"
        (execToOut (Seq (Assign "x" (bool True)) (Print (Var "x"))))
        (Just [Bool True])
  test "print 10"
       (execStmt (Print (Val (Num 10)), empty, [])) 
       (Just (empty, [], [Num 10]))
  test "read then print 42"
       (execToOutWithIn (Seq (Read "x") (Print (Var "x"))) [42])
       (Just [Num 42])
  test "while false print 12"
       (execToOut (While (bool False) (Print (num 12))))
       (Just [])
  test "if true 42 else 0"
       (execToOut (If (bool True) (Print (num 42)) (Print (num 0))))
       (Just [Num 42])
  test "do { print 12 } while false"
       (execToOut (DoWhile (Print (num 12)) (bool False)))
       (Just [Num 12])
  test "for x = 1 to 5 { print x }"
       (execToOut (For "x" (num 1) (num 5) (Print (Var "x"))))
       (Just [Num 1, Num 2, Num 3, Num 4, Num 5])
  test "Array with 5 elements"
       (execToOut (NewArray "array" (num 5) (num 5) `Seq`
                   Set "array" (num 2) (num 42) `Seq`           
                   Print (Get "array" (num 0)) `Seq`
                   Print (Get "array" (num 1)) `Seq`
                   Print (Get "array" (num 2)) `Seq`
                   Print (Get "array" (num 3)) `Seq`
                   Print (Get "array" (num 4))))
       (Just [Num 5, Num 5, Num 42, Num 5, Num 5])
  test "foreach print 0 1 2 3 4"
       (execToOut (NewArray "array" (num 5) (num 0) `Seq`
                   Set "array" (num 1) (num 1) `Seq`
                   Set "array" (num 2) (num 2) `Seq`
                   Set "array" (num 3) (num 3) `Seq`
                   Set "array" (num 4) (num 4) `Seq`
                   ForEach "x" "array" (Print (Var "x"))))
       (Just [Num 0, Num 1, Num 2, Num 3, Num 4])
  test "foreach print 1 1 1 1 1"
        (execToOut (Seq 
          (NewArray "array" (Val (Num 5)) (Val (Num 1)))
          (ForEach "x" "array" (Print (Var "x")))))
        (Just [Num 1,Num 1,Num 1,Num 1,Num 1])
 
