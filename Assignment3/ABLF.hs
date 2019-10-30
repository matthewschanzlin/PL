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
              | LetFun Variable [Variable] ABLFExpr ABLFExpr
              -- function application
              | Call Variable [ABLFExpr]

translate :: ABLFExpr -> Lambda
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
translate (Let v e1 e2) = App (Lam "x" (translate e2)) (translate e1)

translate (Call var (expr:[])) = (App (Var var) (translate expr))
translate (Call var exprs) = (Call (App (Var var) (translate (head exprs))) (tail exprs))

translate (LetFun var vars e1 e2) = (Var "this is so hard")

factorialOf :: Integer -> ABLFExpr
factorialOf 0 = (Num 1)
facotrialOf 1 = (Num 1)
facotrialOf n = (Mul (factorialOf (Sub (Num n) (Num 1))) (Num n)) 
---- tests

tests :: IO ()
tests = do
  test "translate (Num 10)"
       (fromNumeral (translate (Num 10)))
       (Just 10)


---------------------------- your helper functions --------------------------
