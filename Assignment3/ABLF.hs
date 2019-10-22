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
translate _ = undefined

factorialOf :: Integer -> ABLFExpr
factorialOf n = undefined

---- tests

tests :: IO ()
tests = do
  test "translate (Num 10)"
       (fromNumeral (translate (Num 10)))
       (Just 10)


---------------------------- your helper functions --------------------------

