
-- The directive `type` introduces _type synonyms_.
-- In the remainder of this file, Vars and Strings can be 
-- used interchangeably. In fact String is defined using
-- `type String = [Char]`, that is, a String is just a list
-- of characters.
type Vars = String

-- An example of arithmetic expressions with variables a
-- simple binding construct (let).
--
-- A corresponding abstract syntax BNF (using infix
-- notation, similar to Haskell or OCaml) could be:
--
-- <AE> ::= <AE> `+` <AE>
--        | <AE> `-` <AE>
--        | let <Var> = <AE> in <AE>
--        | <Var>
--        | <Int>
data AExpr = Add AExpr AExpr 
           | Sub AExpr AExpr
           | Let Vars AExpr AExpr 
          -- let (x :: Vars) = (ae :: AExpr) in (ae :: AExpr
           | Var Vars
           | Num Integer
           deriving (Eq, Show)
           -- Note: Here we are telling Haskell that we want to be able to 
           --   1. determine the equality of arithmetic expressions,
           --   2. be able to convert them to strings (pretty-print)
           --   3. have a basic way of reading them from strings

-- E.g., the concrete program
--
--   let x = 5 + 4 in x - 5
--
-- is represented by this Haskell value
--
-- Let "x" (Add (Num 5) (Num 4)) (Sub (Var "x") (Num 5))

{- If we wanted, we could define our own way of pretty-printing:

instance Show AExpr where
  show (Add ae1 ae2) = "(+ " ++ show ae1 ++ " " ++ show ae2 ++ ")" 
  show (Num i) = show i
-}

-- basic pretty-printing
printAExpr :: AExpr -> String
printAExpr (Add ae1 ae2) = 
  (("(+ " ++ (printAExpr ae1)) ++ " ") ++ (printAExpr ae2) ++ ")"
printAExpr (Num i) = show i

-- An evaluator for our arithmetic expressions with let-binding
eval :: AExpr -> Integer
eval (Add ae1 ae2) = eval ae1 + eval ae2 -- addition and subtraction
eval (Sub ae1 ae2) = eval ae1 - eval ae2 -- not the recursive calls to eval
eval (Num i) = i -- integers are immediate
eval (Let x ae1 ae2) = eval (subst x (eval ae1) ae2)
-- evaluating a let-binding:
--   1. evaluate the _bound_ expression to a value
--   2. substitute the value for the variable in the expression
--   3. evaluate the resulting expression
eval (Var x) = error ("Undefined variable " ++ x) 
-- encountering an unsubstituted variable means error in this example

-- substitution
subst :: Vars -> Integer -> AExpr -> AExpr
subst x v (Var z) | x == z = Num v -- We found the value we were looking for?
                  | otherwise = Var z -- x /= z - not the variable we're looking for
subst _ _ (Num i) = Num i -- nothing to substitute in a number
subst x v (Add ae1 ae2) = Add (subst x v ae1) (subst x v ae2) -- recursive calls
subst x v (Sub ae1 ae2) = Sub (subst x v ae1) (subst x v ae2)
subst x v (Let y ae1 ae2) | x == y = Let y (subst x v ae1) ae2
                          | otherwise = Let y (subst x v ae1) (subst x v ae2) 
-- There are two cases for let:
--   1. If the variable to be substituted for is the same as the variable being
--      bound in the let expression, we only substitute in the bound expression.
--      Otherwise we would be shadowing the variable being defined.
--   2. If the variable to be substituted for is different, we are free to
--      substitute in both sub-expressions.      
--
-- I will explain more about the scope and visibility of variables in the next
-- lecture.


