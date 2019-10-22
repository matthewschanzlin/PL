{- |
Module      :  Church
Description :  Church encodings of booleans and natural numbers. 
               Fixpoint operator.

Maintainer  :  Your Name <your email>
-}


module Church where

import Lambda
import Reduce

import SimpleTests

toChurchBool :: Bool -> Lambda
toChurchBool _ = undefined

fromChurchBool :: Lambda -> Maybe Bool
fromChurchBool _ = undefined

toNumeral :: Integer -> Lambda
toNumeral _ = undefined

fromNumeral :: Lambda -> Maybe Integer
fromNumeral _ = undefined

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
cplus = undefined

cminus :: Lambda   -- subtraction
cminus = undefined

ctimes :: Lambda   -- multiplication
ctimes = undefined

-- operations on Church booleans
cand :: Lambda
cand = undefined

cor :: Lambda 
cor = undefined

cnot :: Lambda
cnot = undefined

-- operations on numerals returning Church booleans
ciszero :: Lambda
ciszero = undefined

cleq :: Lambda     -- less or equal
cleq = undefined

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
  test "toNumeral -> fromNumeral"
       (fromNumeral (toNumeral 10)) 
       (Just 10)
  -- ...
  test "plus 3 5 --> 8"
       (fromNumeral (normalize (App (App cplus (toNumeral 3)) (toNumeral 5))))
       (Just (3 + 5))


---------------------------- your helper functions --------------------------

