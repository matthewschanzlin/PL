{- |
Module      :  Church
Description :  Church encodings of booleans and natural numbers. 
               Fixpoint operator.

Maintainer  :  Nicholas Seidl <Nicholas Seidl seidl.n@husky.neu.edu>, Matthew Schanzlin <Matthew Schanzlin <schanzlin.ma@husky.neu.edu>
-}


module Church where

import Lambda
import Reduce

import SimpleTests

toChurchBool :: Bool -> Lambda
toChurchBool b = 
  if b == True 
    then (Lam "t" (Lam "f" (Var "t")))
    else (Lam "t" (Lam "f" (Var "f")))

fromChurchBool :: Lambda -> Maybe Bool
fromChurchBool (Lam var (Lam var' (Var var''))) =
  if var == var''
    then Just True
    else Just False
fromChurchBool _ = Nothing

toNumeral :: Integer -> Lambda
toNumeral 0 = (Var "z")
toNumeral n = (Lam "s" (toNumeral (n - 1)))

fromNumeral :: Lambda -> Maybe Integer
fromNumeral (Var var) = Just 0
fromNumeral (Lam var lam) = 
  case fromNumeral lam of
    Just v -> Just (1 + v)
    Nothing -> Nothing
fromNumeral _ = Nothing

csucc :: Lambda
csucc = Lam "n" (Lam "s" (Lam "z" 
        (App (Var "s") (App (App (Var "n") (Var "s")) (Var "z")))))

cpred :: Lambda
cpred = Lam "n" (Lam "f" (Lam "x" (
          App (
            App (
              App (Var "n") 
                  (Lam "g" (Lam "h" (
                    App (Var "h") (App (Var "g") (Var "f")))))) 
              (Lam "u" (Var "x"))) 
            (Lam "u" (Var "u")))))

-- operations on numerals
cplus :: Lambda    -- addition
cplus = (Lam "m" (Lam "n" (App (Var "m") csucc)))

cminus :: Lambda   -- subtraction
cminus = (Lam "m" (Lam "n" (App cpred (Var "m"))))

ctimes :: Lambda   -- multiplication
ctimes = (Lam "m" (Lam "n" (App (Var "m") csucc)))

-- operations on Church booleans
cand :: Lambda
cand = (Lam "a" (Lam "b" (App (App (Var "a") (Var "b")) (toChurchBool False))))

cor :: Lambda 
cor = (Lam "a" (Lam "b" (App (App (Var "a") (toChurchBool True)) (Var "b"))))

cnot :: Lambda
cnot = (Lam "b" (App (App (Var "b") (toChurchBool False)) (toChurchBool True)))

-- operations on numerals returning Church booleans
ciszero :: Lambda
ciszero = (Lam "n" (App (App (Var "n") (Lam "x" (toChurchBool False))) (toChurchBool True)))

cleq :: Lambda     -- less or equal
cleq = (Lam "m" (Lam "n" (App ciszero cminus)))

ceq :: Lambda      -- equal
ceq = undefined

-- conditional expression
cifthen :: Lambda
cifthen = undefined

-- fixpoint combinator
fix :: Lambda
fix = undefined

------ tests go here

tests :: IO ()
tests = do
  --test "toNumeral -> fromNumeral"
  --     (fromNumeral (toNumeral 10)) 
  --     (Just 10)
  ---- ...
  --test "plus 3 5 --> 8"
  --     (fromNumeral (normalize (App (App cplus (toNumeral 3)) (toNumeral 5))))
  --     (Just (3 + 5))
  test "toChurchBool True"
    (toChurchBool True)
    (Lam "t" (Lam "f" (Var "t")))
  test "toChurchBool False"
    (toChurchBool False)
    (Lam "t" (Lam "f" (Var "f")))
  test "fromChurchBool churchFalse"
    (fromChurchBool (Lam "t" (Lam "f" (Var "f"))))
    (Just False)
  test "fromChurchBool chuchTrue"
    (fromChurchBool (Lam "t" (Lam "f" (Var "t"))))
    (Just True)
  test "fromChurchBool nonCanonical"
    (fromChurchBool (Lam "t" (Lam "f" (Lam "q" (Var "t")))))
    (Nothing)
---------------------------- your helper functions --------------------------

