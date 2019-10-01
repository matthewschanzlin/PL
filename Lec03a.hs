
-- Evaluating to different types of values
-- - involves dynamic type checking of value types

import Env -- implement get, add, empty


data Value = Num Integer
           | Bl Bool
           deriving (Eq, Show)

data BAExpr = Add BAExpr BAExpr
            | And BAExpr BAExpr
            | Not BAExpr 
            | Let String BAExpr BAExpr
            | Var String
            | Val Value 
            deriving (Eq, Show)

eval :: Env Value -> BAExpr -> Value
eval e (Add ae1 ae2) = 
  case eval e ae1 of
       Num v1 -> case eval e ae2 of
                      Num v2 -> Num (v1 + v2)
                      _ -> error "Add: right operand has wrong type"
       _ -> error "Add: left operand has wrong type"
eval e (And ae1 ae2) = 
  case eval e ae1 of
       Bl b1 -> case eval e ae2 of 
                     Bl b2 -> Bl (b1 && b2)
                     _ -> error "And: right operand has wrong type"
       _ -> error "And: left operand has wrong type"
eval e (Not ae) = 
  case eval e ae of
       Bl b -> Bl (not b)
       _ -> error "Not: operand has wrong type"
eval e (Val v) = v
eval e (Let x ae1 ae2) = 
  let v1 = eval e ae1
      e' = add x v1 e
  in eval e' ae2
eval e (Var x) = get x e




