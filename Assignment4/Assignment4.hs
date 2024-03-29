{- |
Module      :  Assignment4
Description :  Assignment 4 meta-file.

Maintainer  :  Nicholas Seidl <seidl.n@husky.neu.edu>, Matthew Schanzlin <schanzlin.ma@husky.neu.edu>
-}
module Assignment4 where

import qualified MiniImp

-- Fill in your name(s)
-- For a single partner: (Just "Your Name", Nothing)
-- For a pair: (Just "First Partner", Just "Second Partner")
partners :: (Maybe String, Maybe String)
partners = (Just "Matthew Schanzlin", Just "Nicholas Seidl")

-- Please provide the total hours spent on this assignment
hoursSpent :: (Int, Int)
hoursSpent = (0, 0)

---------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "== Testing module MiniImp"
  MiniImp.tests

