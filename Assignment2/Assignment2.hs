{- |
Module      :  Assignment2
Description :  Assignment 2 meta-file.

Maintainer  :  Matthew Schanzlin <schanzlin.ma@husky.neu.edu> Nicholas Seidl <seidl.n@husky.neu.edu>
-}

import qualified Env (tests)
import qualified ABL (tests)
import qualified StrictEnvABL (tests)

-- Fill in your name(s)
-- For a single partner: (Just "Your Name", Nothing)
-- For a pair: (Just "First Partner", Just "Second Partner")
partners :: (Maybe String, Maybe String)
partners = (Just "Matthew Schanzlin", Just "Nicholas Seidl")

-- Please provide the total hours spent on this assignment
hoursSpent :: (Int, Int)
hoursSpent = (10, 10)


---------------------------------------------------------------

allTests :: IO ()
allTests = do
  Env.tests
  ABL.tests
  StrictEnvABL.tests
