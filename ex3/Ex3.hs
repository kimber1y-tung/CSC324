{-|
Module: Ex3
Description: Exercise 3

This part of the exercise is similar in spirit to Exercise 2, except using
the Haskell programming language instead.

The one /new/ idea here is the use of the QuickCheck library to use a different
kind of approach to testing called /property-based testing/. We illustrate a
few simple property-based tests throughout this file.
-}
-- This lists what this module exports. Don't change this!
module Ex3
  (
    calculate
  )
where

-- You *may* add imports from Data.List for task 3 (but no other imports).
import Data.List (sort)
import Test.QuickCheck (Property, quickCheck, (==>))
import Ex3Types (Expr(..))

-------------------------------------------------------------------------------
-- |
-- * Task 3: Calculator in Haskell
-------------------------------------------------------------------------------

-- | calculate: take an Expr, and evaluate it to return a number
-- Here you'll need to use pattern-matching on the different forms of an Expr 
-- (@Number@ or @Add@ or @Sub@ ...), because we don't have an explicit "number?"
-- function for this datatype.
calculate :: Expr -> Float
calculate (Number n) =
    -- In this case, the expression is a number
    (n)
calculate (Add expr1 expr2) =
    -- In this case, the expression is an addition
    calculate (expr1) + calculate (expr2)
-- TODO: complete the rest of the function

calculate (Sub expr1 expr2) =
    -- In this case, the expression is a subtraction 
    calculate (expr1) - calculate (expr2)

calculate (Div expr1 expr2) =
    -- In this case, the expression is a subtraction 
    calculate (expr1) / calculate (expr2)

calculate (Mul expr1 expr2) =
    -- In this case, the expression is a subtraction 
    calculate (expr1) * calculate (expr2)

-------------------------------------------------------------------------------
-- | This property checks that (calculate expr) is always one less than
-- | than (calculate (Add (Number 1) expr))
prop_calculateAdd :: Expr -> Bool
prop_calculateAdd expr = ((calculate expr) + 1) == (calculate (Add (Number 1) expr))

-- | What do you think this property says?
prop_calculateSub:: Expr -> Bool
prop_calculateSub expr = ((calculate expr) - 1) == (calculate (Sub expr (Number 1)))

-- | What do you think this property says?
prop_calculateDouble:: Expr -> Bool
prop_calculateDouble expr = ((calculate expr) * 2) == (calculate (Mul (Number 2) expr))

-- | What do you think this property says?
prop_calculateHalf :: Expr -> Bool
prop_calculateHalf expr = ((calculate expr) / 2) == (calculate (Div expr (Number 2)))

prop_calculateNumber :: Bool
prop_calculateNumber = calculate (Number 2) == 2


prop_calculateNumber2 :: Bool
prop_calculateNumber2 = calculate (Add (Number 1) (Number 2)) == 3



-------------------------------------------------------------------------------
-- * Main function (for testing purposes only)
-------------------------------------------------------------------------------

-- This main function is executed when you compile and run this Haskell file.
-- It runs the QuickCheck tests; we'll talk about "do" notation much later in
-- the course, but for now if you want to add your own tests, just define them
-- above, and add a new `quickCheck` line below.
main :: IO ()
main = do
  quickCheck prop_calculateNumber   -- new test!
  quickCheck prop_calculateNumber2  -- new test!
  quickCheck prop_calculateAdd
  quickCheck prop_calculateSub
  quickCheck prop_calculateDouble
  quickCheck prop_calculateHalf


