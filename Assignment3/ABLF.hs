{- |
Module      :  ABLF
Description :  Syntax of the ABLF language and a translation function to pure
               lambda calculus.

Maintainer  :  Nicholas Seidl <seidl.n@husky.neu.edu>, Matthew Schanzlin <schanzlin.ma@husky.neu.edu>
-}


module ABLF where

import Lambda
import Church

import Reduce   -- for testing

import SimpleTests


data ABLFExpr = AVar Variable
              | Num Integer             -- operations on naturals
              | Add ABLFExpr ABLFExpr
              | Sub ABLFExpr ABLFExpr
              | Mul ABLFExpr ABLFExpr

              | Bool Bool               -- operations on booleans
              | And ABLFExpr ABLFExpr
              | Or ABLFExpr ABLFExpr
              | Not ABLFExpr

              | Leq ABLFExpr ABLFExpr   -- Leq n m: "Is n less or equal to m?"
              | Eq ABLFExpr ABLFExpr    -- Eq n m: "Is n equal to m?"

              | IfThen ABLFExpr ABLFExpr ABLFExpr  -- conditional expression

              | Let Variable ABLFExpr ABLFExpr     -- let x = e1 in e2

              -- recursive function definitions
              | LetFun Variable [Variable] ABLFExpr ABLFExpr -- let x a b c = e1 in e2
              -- function application
              | Call Variable [ABLFExpr] -- f(x, y) = f (f)

translate :: ABLFExpr -> Lambda
translate (AVar var) = Var var
translate (Num i) = toNumeral i
translate (Add e1 e2) = App (App cplus (translate e1)) (translate e2)
translate (Sub e1 e2) = App (App cminus (translate e1)) (translate e2)
translate (Mul e1 e2) = App (App ctimes (translate e1)) (translate e2)

translate (Bool b) = toChurchBool b
translate (And e1 e2) = App (App cand (translate e1)) (translate e2)
translate (Or e1 e2) = App (App cor (translate e1)) (translate e2)
translate (Not e1) = App cnot (translate e1)

translate (Leq e1 e2) = App (App cleq (translate e1)) (translate e2)
translate (Eq e1 e2) = App (App ceq (translate e1)) (translate e2)
translate (IfThen e1 e2 e3) = App (App (App cifthen (translate e1)) (translate e2)) (translate e3)
translate (Let v e1 e2) = App (Lam v (translate e2)) (translate e1)

translate (Call var []) = Var var
translate (Call var exprs) = App (translate (Call var (init exprs))) (translate (last exprs))

-- let rec f x y z = e1 in e2 LetFun "f" ["x", "y", "z"] e1 e2
-- LetFun "f" ["x", "y", "z"] e1 e2
-- let rec f x y z = e1 in e2
-- let rec f = lam x y z. e1 in e2
-- let f = fix (λf. λx y z. e1) in e2
-- (λf. e2) (fix (λf. λx y z. e1))
translate (LetFun var [] e1 e2) = translate (Let var e1 e2)
translate (LetFun var vars e1 e2) = App (Lam var (translate e2))
                                        (App fix (Lam var (mulArgToSingleArg vars e1)))

mulArgToSingleArg :: [Variable] -> ABLFExpr -> Lambda
mulArgToSingleArg [] e = translate e
mulArgToSingleArg vars e = (Lam (head vars) (mulArgToSingleArg (tail vars) e))

factorialOf :: Integer -> ABLFExpr
factorialOf n = (LetFun "fact" ["n"] (IfThen (Eq (Num 0) (AVar "n"))
                                      (Num 1)
                                      (Mul (AVar "n") (Call "fact" [(Sub (AVar "n") (Num 1))])))
                                    (Call "fact" [(Num n)]))

tests :: IO ()
tests = do
  test "translate (Num 10)"
       (fromNumeral (translate (Num 10)))
       (Just 10)
  test "translate 4-2"
    (fromNumeral (normalize (translate (Sub (Num 4) (Num 1)))))
    (Just 3)
  test "translate Add"
    (fromNumeral (normalize (translate (Add (Num 4) (Num 1)))))
    (Just 5)
  test "translate Mul"
    (fromNumeral (normalize (translate (Mul (Num 4) (Num 1)))))
    (Just 4)
  test "translate Bool"
    (fromNumeral (normalize (translate (Bool True))))
    Nothing
  test "translate And"
    (fromNumeral (normalize (translate (And (Eq (Num 4) (Num 1)) (Eq (Num 4) (Num 1))))))
    (Just 0)
  test "translate Or"
    (fromNumeral (normalize (translate (Or (Eq (Num 4) (Num 1)) (Eq (Num 4) (Num 1))))))
    (Just 0)
  test "translate Not"
    (fromNumeral (normalize (translate (Not (Eq (Num 4) (Num 1))))))
    Nothing
  test "translate Leq"
    (fromNumeral (normalize (translate (Leq (Num 4) (Num 1)))))
    (Just 0)
  test "translate Eq"
    (fromNumeral (normalize (translate (Eq (Num 4) (Num 1)))))
    (Just 0)
  test "translate Let"
    (fromNumeral (normalize (translate (Let "v" (Num 4) (Num 1)))))
    (Just 1)
  test "call"
        (translate (Call "f" [(AVar "x"), (AVar "y"), (AVar "z")]))
        (App (App (App (Var "f") (Var "x")) (Var "y")) (Var "z"))
  test "letfun basic"
        (translate (LetFun "f" ["x"] (AVar "a") (AVar "b")))
        (App (Lam "f" (Var "b")) (App (Lam "f" (App (Lam "x" (App (Var "f") (App (Var "x") (Var "x")))) (Lam "x" (App (Var "f") (App (Var "x") (Var "x")))))) (Lam "f" (Lam "x" (Var "a")))))
  test "letfun advanced"
        (translate (LetFun "f" ["x"] (Num 0) (Call "f" [(AVar "x")])))
        (App (Lam "f" (App (Var "f") (Var "x"))) (App (Lam "f" (App (Lam "x" (App (Var "f") (App (Var "x") (Var "x")))) (Lam "x" (App (Var "f") (App (Var "x") (Var "x")))))) (Lam "f" (Lam "x" (Lam "s" (Lam "z" (Var "z")))))))
  test "fact 0"
        (fromNumeral (normalize (translate (factorialOf 0))))
        (Just 1)
  test "fact 1"
        (fromNumeral (normalize (translate (factorialOf 1))))
        (Just 1)
  test "fact 2"
        (fromNumeral (normalize (translate (factorialOf 2))))
        (Just 2)
---------------------------- your helper functions --------------------------
  test "mulArgToSingleArg base case"
        (mulArgToSingleArg [] (Num 2))
        (Lam "s" (Lam "z" (App (Var "s") (App (Var "s") (Var "z")))))
  test "mulArgToSingleArg hard"
        (mulArgToSingleArg ["x", "y", "z", "b", "c"] (Num 2))
        (Lam "x" (Lam "y" (Lam "z" (Lam "b" (Lam "c" (Lam "s" (Lam "z" (App (Var "s") (App (Var "s") (Var "z"))))))))))
  test "mulArgToSingleArg basic"
        (mulArgToSingleArg ["x"] (Num 2))
        (Lam "x" (Lam "s" (Lam "z" (App (Var "s") (App (Var "s") (Var "z"))))))
  test "mulArgToSingleArg advanced"
        (mulArgToSingleArg ["x", "y", "z"] (Num 2))
        (Lam "x" (Lam "y" (Lam "z" (Lam "s" (Lam "z" (App (Var "s") (App (Var "s") (Var "z"))))))))
