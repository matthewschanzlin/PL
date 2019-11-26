{- |
Module      :  Assignment5
Description :  Assignment 5 meta-file.

Maintainer  :  Your Name <your email>
-}
module Assignment5 where

import qualified Types
import qualified StlcExt

-- Fill in your name(s)
-- For a single partner: (Just "Your Name", Nothing)
-- For a pair: (Just "First Partner", Just "Second Partner")
partners :: (Maybe String, Maybe String)
partners = undefined

-- Please provide the total hours spent on this assignment
hoursSpent :: (Int, Int)
hoursSpent = (0, 0)

---------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "== Testing module Types"
  Types.tests
  putStrLn "== Testing module StlcExt"
  StlcExt.tests

