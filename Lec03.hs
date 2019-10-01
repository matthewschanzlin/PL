
-- Evaluating with environments

import Env -- implements get, add, empty

data AExpr = Add AExpr AExpr
           | Num Integer
           | Let String AExpr AExpr
           | Var String
           deriving (Eq, Show)


eval :: (Env Integer) -> AExpr -> Integer
eval e (Add ae1 ae2) = eval e ae1 + eval e ae2
eval e (Num i) = i
eval e (Let x ae1 ae2) = 
  let v1 = eval e ae1  -- evaluate the bound expression (ae1)
      e' = add x v1 e  -- update the environment with the value
  in eval e' ae2       -- evaluate ae2 using the updated environment
eval e (Var x) = get x e  -- find the appropriate binding

{-
  Environments can have differen concrete implementations
  as long as they have operations for 
    1. creating an empty environment,
    2. extending an environment with a binding (add), and
    3. finding a binding in an environment.

  The important thing is that the implementations satisfy
  the following two axioms:

    get "x" (add "x" v e) = v
    get "x" (add "y" v e) = get "x" e 

  Here is an example evaluation:

  eval empty (Let "x" (Num 10) (Let "y" (Num 20) (Add (Var "x") (Var "y"))))
    = let v1 = eval empty (Num 10)
          e' = add "x" v1 empty
      in eval e' (Let "y" (Num 20) (Add (Var "x") (Var "y")))
    = let v1 = 10
          e' = add "x" 10 empty
      in eval e' (Let "y" (Num 20) (Add (Var "x") (Var "y")))
    = in eval (add "x" 10 empty) (Let "y" (Num 20) (Add (Var "x") (Var "y")))
    = in eval (add "x" 10 empty) (Let "y" (Num 20) (Add (Var "x") (Var "y")))
    = let v1 = eval (add "x" 10 empty) (Num 20)
          e' = add "y" v1 (add "x" 10 empty)
      in eval e' (Add (Var "x") (Var "y")) 
    = let v1 = 20
          e' = add "y" 20 (add "x" 10 empty)
      in eval e' (Add (Var "x") (Var "y")) 
    = eval (add "y" 20 (add "x" 10 empty)) (Add (Var "x") (Var "y")) 
    = eval (add "y" 20 (add "x" 10 empty)) (Var "x")
      + 
      eval (add "y" 20 (add "x" 10 empty)) (Var "y")
    = get "x" (add "y" 20 (add "x" 10 empty))
      +
      get "y" (add "y" 20 (add "x" 10 empty))
    = get "x" (add "x" 10 empty)
      +
      20
    = 10 + 20
    = 30

    eval empty (Let "x" (Num 10) (Let "x" (Num 20) (Var "x")))
    = eval (add "x" 10 empty) (Let "x" (Num 20) (Var "x"))
    = eval (add "x" 20 (add "x" 10 empty)) (Var "x")
    = get "x" (add "x" 20 (add "x" 10 empty))
    = 20 

-}
