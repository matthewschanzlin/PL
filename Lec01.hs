
-- Libraries/modules can be imported using the obviously named `import`
--
-- Debug.Trace contains the `trace` function, particularly useful for 
-- debugging pure functions.
import Debug.Trace


-- Example of a basic function definition 
doubleMe :: Integer -> Integer -- type signature
doubleMe x = x + x             -- one ore more defining equations

-- Recursive functions
-- In Haskell, every definition is recursive by default. Furthermore, 
-- everything is mutually recursive within a particular scope, so definitions
-- can refer to other definitions defined below in the file.
--
-- Factorial variant using a conditional
fact :: Integer -> Integer
fact n = if n < 2 then 1 else n * fact (n - 1)

-- _Pattern matching_ is performed using case ... of
fact' :: Integer -> Integer
fact' n = 
  case n of
       0 -> 1
       _ -> n * fact' (n - 1) 

-- _Pattern matching_ can be used directly on the left-hand side of the
-- _defining equations_. There can be multiple equations, but they have to be 
-- grouped together, otherwise Haskell will think they are separate definitions 
-- of the same name.
fact'' :: Integer -> Integer
fact'' 0 = 1
fact'' n = n * fact'' (n - 1)

-- _Guards_ are for conditional pattern matching.
fact''' :: Integer -> Integer
fact''' n | n < 2 = 1 -- only match if n < 2
          | otherwise = n * fact''' (n - 1) -- "otherwise, this case applies"

n = 20


-- _Algebraic datatypes_ are introduced using `data`, followed by a type 
-- constructor. In the example bellow, `Test` is a _type constructor_, `This` 
-- and `Other` are _data constructors_ (or _value constructors_). `This` takes 
-- an Int as an argument to construct a value, while Other takes a String and a
-- Test. Since Other takes itself value of type Test, the type being defined, 
-- Test is a recursive datatype.
data Test = This Int
          | Other String Test

-- Example function over an algebraic datatype.
testToNum :: Test -> Int
testToNum (This n) | n > 10 = n
                   | otherwise = 5
testToNum (Other s _) = 0 -- when we do not care about a value, we can use 
                          -- the underscore _ in a pattern, to avoid binding a 
                          -- name

-- A recursive datatype does not have to have a base case. The following 
-- definition defines an infinite datatype - there is no way of terminating a 
-- stream. Compare with the definition of a list below.
data StringStream = StringStream String StringStream 
-- N.B. Both the type constructor and a data constructor can have the same 
-- name - they live in different namespaces. This is often used in Haskell for
-- datatypes with a single constructor.

-- A string list datatype
data MyStringList = MyStringNil
                  | MyStringCons String MyStringList

-- A polymorphic datatype. Type constructors can take a type argument. The 
-- argument can be used in the definition of the type.
data MyList a = MyNil
              | MyCons a (MyList a)
-- You can think of polymorphic (parametrized) type constructors as functions
-- that take a type as an argument and produce type. E.g. `MyList String` is
-- a MyList containing strings. Further examples, `MyList Integer`, 
-- MyList (MyList String).

-- Conventions: type variables names are usually taken from the 
-- beginning of the alphabet

-- Type constructors can take multiple arguments. Here are polymorphic pairs, 
-- where each element can be a different type.
data MyPair a b = MyPair a b

-- (Polymorphic) Functions over polymorphic types.
myLength :: MyList a -> Integer
myLength MyNil = 0
myLength (MyCons _ l) = 1 + myLength l

myHead :: MyList a -> a
myHead (MyCons x _) = x

myTail :: MyList a -> MyList a
myTail (MyCons _ l) = l
myTail MyNil = MyNil

myLast :: MyList a -> a
myLast (MyCons x MyNil) = x
myLast (MyCons _ l) = myLast l
myLast _ = error "This case is undefined!!!"
myLast _ = undefined

-- The last two equations show two easy ways of signalling undefinedness, which
-- come in handy during development.

-- Haskell's standard lists
-- Haskell uses "fancy" notation for some of its types as well values. Thus, 
-- for example, the type of lists is written [a], where a is a type. For
-- example, [String] is the type of lists of strings. The empty list is written
-- [], prepending an element to a list is written (x : l). Syntactic sugar is 
-- provided for constructing list values.
--
-- Examples:
--    1 : [] = [1]
--    1 : 2 : [] = 1 : [2] = [1, 2]
-- etc.
length' :: [a] -> Integer
length' [] = 0
length' (_ : l) = 1 + length' l

-- The `where` keyword introduces local bindings for a defining equation.
f :: Integer -> Integer
f n = x * y
  where x = n + n
        y = n `div` 2

-- "Imperative-style" programming with side effects is performed within the
-- IO type (this is a simplification, but it's good enough for now). We will
-- look at the why and how later, for now, remember that any function
-- (computation), that does something with the "outside world", will have an 
-- IO type. Note that a function can both do IO and return a value, such as 
-- the function below. Also, for now, think of the `do` keyword as 
-- introducing a sequence of computations with side effects.
hello :: Integer -> IO Integer
hello n = do
  let message = "Hello, " -- _pure_ values are bound using let in a do block
  name <- getLine -- getting the value returned by an IO computation
  putStrLn (message ++ name)
  return n -- return a value from an IO function




