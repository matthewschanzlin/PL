{- |
Module      :  Assignment2
Description :  Assignment 2 meta-file.

Maintainer  :  Your Name <your email>
-}

import qualified Env (tests)
import qualified ABV (tests)
import qualified StrictEnvEBL (tests)

-- Fill in your name(s)
-- For a single partner: (Just "Your Name", Nothing)
-- For a pair: (Just "First Partner", Just "Second Partner")
partners :: (Maybe String, Maybe String)
partners = undefined

-- Please provide the total hours spent on this assignment
hoursSpent :: (Int, Int)
hoursSpent = (0, 0)


---------------------------------------------------------------

allTests :: IO ()
allTests = do
  Env.tests
  ABV.tests
  StrictEnvEBL.tests

