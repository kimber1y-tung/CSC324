{-|
Module: Ex1
Description: Exercise 1: Getting Started with Racket and Haskell

This part of the exercise is similar in spirit to the Racket part, except using
the Haskell programming language instead.

We strongly recommend completing the functions in Racket (and verifying their
correctness) first, so that way your work in this part can be done by translating
the syntax of Racket into Haskell.

The one /new/ idea here is the use of the QuickCheck library to use a different
kind of approach to testing called /property-based testing/. We illustrate a few simple property-based tests throughout this file.
-}
-- This lists what this module. Don't change this!
module Ex1
  ( celsiusToFahrenheit
  , nCopies
  , numEvens
  , numManyEvens
  )
where

-- Imports used for testing purposes only.
import Test.QuickCheck (Property, quickCheck, (==>))

-------------------------------------------------------------------------------
-- * Note about type signatures
--
-- Unlike Racket, Haskell is /statically-typed/. We'll go into more detail about
-- what this means later in the course, but for now we've provided type signatures
--  for the functions here to simplify any compiler error messages you might
-- receive. (Don't change them; they're required to compile against our tests.)
-------------------------------------------------------------------------------

-- | Converts a temperature from Celsius to Fahrenheit.
-- __Note__: use the @round@ function to convert from floating-point types
-- to @Int@.
celsiusToFahrenheit :: Float -> Int
celsiusToFahrenheit temp =
    -- TODO: replace `undefined` with a proper function body.
    (round (temp * (9 / 5) + 32))

-- | The simplest "property-based test" is simply a unit test; note the type.
prop_celsius0 :: Bool
prop_celsius0 = celsiusToFahrenheit 0 == 32
prop_celsius37 :: Bool
prop_celsius37 = celsiusToFahrenheit 37 == 99

-------------------------------------------------------------------------------
-- * Recursion with numbers
--
-- For the recursive functions, we recommend doing these in two ways:
--
--   1. First, write them using @if@ expressions, as you would in Racket.
--   2. Then when that works, use /pattern-matching/ to simplify the definitions
--      (<http://learnyouahaskell.com/syntax-in-functions#pattern-matching>).
--
-- Remember: Strings are simply lists of characters. (@String === [Char]@)
-- Read more about manipulating lists at
-- <http://learnyouahaskell.com/starting-out#an-intro-to-lists>.

-- | Returns a new string that contains @n@ copies of the input string.
nCopies :: String -> Int -> String
nCopies s 0 = ""
nCopies s n = s ++ nCopies s (n - 1)


-- | This is a QuickCheck property that says,
-- "If n >= 0, then when you call nCopies on a string s and int n,
-- the length of the resulting string is equal to
-- n * the length of the original string."
--
-- QuickCheck verifies this property holds for a random selection of
-- inputs (by default, choosing 100 different inputs).
prop_nCopiesLength :: String -> Int -> Property
prop_nCopiesLength s n = n >= 0 ==> length (nCopies s n) == (length s * n)

-------------------------------------------------------------------------------
-- * Recursion with lists
-------------------------------------------------------------------------------

-- | Returns the number of even elements in the given list.
--
-- We've given you a recursive template here to start from.
-- But noted as above, you can later try simplifying this definition
-- using pattern matching.
numEvens :: [Int] -> Int
numEvens numbers = 
  if null numbers 
    then 0
    else
      let firstNumber = head numbers
          restNumber = numEvens (tail numbers)
      in 
        if even firstNumber
          then 1 + restNumber
          else restNumber


--  if null numbers
--     then
--       ...
--     else
--       let firstNumber = head numbers
--           pickBetterName = numEvens (tail numbers)
--       in
--         if even firstNumber
--           then 


-- | Returns the number of inner lists that contain three or more even integers.
numManyEvens :: [[Int]] -> Int
numManyEvens listsOfNums =
    if null listsOfNums 
    then 0
    else
      let firstNumber = head listsOfNums
          restNumber = numManyEvens (tail listsOfNums)
      in 
        if numEvens firstNumber >= 3
          then 1 + restNumber
          else restNumber


-- | This is a property that says, "the number returned by numEvens is
-- less than or equal to the length of the original list."
prop_numEvensLength :: [Int] -> Bool
prop_numEvensLength nums = numEvens nums <= length nums

-- | What do you think this property says?
prop_numManyEvensDoubled :: [[Int]] -> Bool
prop_numManyEvensDoubled listsOfNums =
  let doubled = listsOfNums ++ listsOfNums
  in numManyEvens doubled == 2 * numManyEvens listsOfNums

-------------------------------------------------------------------------------
-- * Main function (for testing purposes only)
-------------------------------------------------------------------------------

-- This main function is executed when you compile and run this Haskell file.
-- It runs the QuickCheck tests; we'll talk about "do" notation much later in
-- the course, but for now if you want to add your own tests, just define them
-- above, and add a new `quickCheck` line below.
main :: IO ()
main = do
  quickCheck prop_celsius0
  quickCheck prop_celsius37
  quickCheck prop_nCopiesLength
  quickCheck prop_numEvensLength
  quickCheck prop_numManyEvensDoubled
