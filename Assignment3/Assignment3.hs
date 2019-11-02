{- |
Module      :  Assignment3
Description :  Assignment 3 meta-file.

Maintainer  :  Nicholas Seidl <seidl.n@husky.neu.edu>, Matthew Schanzlin <schanzlin.ma@husky.neu.edu>
-}
module Assignment3 where

import qualified ABLF (tests)
import qualified Church (tests)

-- Fill in your name(s)
-- For a single partner: (Just "Your Name", Nothing)
-- For a pair: (Just "First Partner", Just "Second Partner")
partners :: (Maybe String, Maybe String)
partners = (Just "Matthew Schanzlin", Just "Nicholas Seidl")

-- Please provide the total hours spent on this assignment
hoursSpent :: (Int, Int)
hoursSpent = (20, 20)

-- For Exercise 7, for each number below, fill in how many reduction steps a 
-- factorial of that number needed to reach a normal form.
factorialSteps =
  [ (1, 104)
  , (2, 393)
  , (3, 1824)
  , (4, 10207)
  , (5, 67274)
  ]
---------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "== Testing module Church"
  Church.tests
  putStrLn "== Testing module ABLF"
  ABLF.tests

