{-|
Module: Ex4Types
Description: Types for Exercise 4

This module provides the public types required for the Haskell portion of
Exercise 4. You should review the data type Expr carefully, but do not
change anything in this file! We will use a fresh copy of this file
for testing purposes.
-}
module Ex4Types
  (
    Value(..),
    Expr(..)
  )
where

import Data.Map
import Control.Monad (liftM2)

-------------------------------------------------------------------------------
-- * Task 2 (Expr type)
-------------------------------------------------------------------------------

data Value = Number Float
           | Closure String Expr (Map String Value)
           | Error
           deriving (Show, Eq)

data Expr = Literal Value   -- ^ literal value (number or closure)
          | Id String       -- ^ identifiers
          | Add Expr Expr   -- ^ addition
          | Sub Expr Expr   -- ^ subtraction
          | Mul Expr Expr   -- ^ multiplication
          | Div Expr Expr   -- ^ division
          | Let [(String, Expr)] Expr -- ^ let expression
          | Lambda String Expr  -- ^ function expression
          | Apply Expr Expr     -- ^ function application
          deriving (Show, Eq)

-- | Example: the value 20
numberValue :: Value
numberValue = Number 20

-- | Example: the expression that is the literal number 20
exprValue :: Expr
exprValue = Literal $ Number 20 --  same as: Literal (Number 20)

-- | Example: (+ 2 3)
addExample :: Expr
addExample = Add (Literal (Number 2)) (Literal (Number 3))

-- | Example: (+ 2 (- 5 1))
exprExample :: Expr
exprExample = Add (Literal $ Number 2) (Sub (Literal $ Number 5) (Literal $ Number 1))

