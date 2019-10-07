{- |
Module      :  Env
Description :  An implementation of environments as association lists.

Maintainer  :  Your Name <your email>
-}

module Env where

import SimpleTests

type Env a = [(String, a)]

-- Replace `undefined` with the appropriate Haskell expressions.

-- Construct an empty environment
empty :: Env a
empty = undefined {- TASK: replace -}

-- Add a binding of `x` to `v` to the environment `env`.
add :: String -> a -> Env a -> Env a
add x v env = undefined {- TASK: replace -}

-- Retrieve the binding from an environment. If the binding is not found, return
-- Nothing.
get :: String -> Env a -> Maybe a
get x [] = undefined {- TASK: replace -}
get x ((y, v) : env) = undefined {- TASK: replace -}


-- provide your tests
tests :: IO ()
tests = do 
  test "get empty" (get "x" (empty :: Env Integer)) Nothing
  test "get add" (get "x" (add "x" 10 empty)) (Just 10)


---------------------------- your helper functions --------------------------

