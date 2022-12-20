{-|
Module: P1 
Description: Project 1: A Spreadsheet Application with DeerLang
Copyright: (c) University of Toronto Mississagua, 2020
               CSC324 Principles of Programming Languages, Fall 2020
-}
-- This lists what this module exports. Don't change this!
module P1
  (
    evalDeer,
    computeSpreadsheet
  )
where

-- You *may not* add imports from Data.Map 
import P1Types(Spreadsheet(..), Definition(..), Column(..),
               Expr(..), Value(..),
               Env, exampleSpreadsheet)
import Prelude hiding (lookup)
import qualified Data.Map (lookup, insert, empty)

-------------------------------------------------------------------------------
-- Main Functions: 
-- | These are the functions that we will be directly testing.
-- | Do not change the type signatures of these functions.
-------------------------------------------------------------------------------

evalDeer :: Expr -> Env -> Value
evalDeer (Id id) env = case (Data.Map.lookup id env) of
                           Just value -> value
                           Nothing    -> Error
evalDeer (Literal v) _ = v
evalDeer (Builtin op exprs) env = builtinHelper op exprs env
evalDeer (Lambda ids expr) env = (VClosure ids expr env)
evalDeer (Apply expr exprs) env = applyHelper (evalDeer expr env) exprs env

computeSpreadsheet :: Spreadsheet -> [Column]
computeSpreadsheet (Spreadsheet defs columns) =
  let defEnv    = doGetEnvironment defs Data.Map.empty
        -- List comprehension from: https://stackoverflow.com/questions/29046369/how-to-filter-list-by-type-in-haskell
      valueCols = [x | x@(ValCol _ _) <- columns]
      computeCols = [x | x@(ComputedCol _ _) <- columns]
  in computeSpreadsheetHelper valueCols computeCols defEnv

-------------------------------------------------------------------------------
-- Helper Functions
-- | You may add, remove, or modify any helper functions here.
-------------------------------------------------------------------------------
-- evalDeer helpers
-------------------------------------------------------------------------------
-- Translation layer to get all the information needed
applyHelper :: Value -> [Expr] -> Env -> Value
applyHelper (VClosure bindings body closEnv) exprs env
  | length bindings == length exprs = evalDeer body (addBindingsToEnv bindings exprs closEnv env)
  | otherwise = Error
applyHelper _ _ _ = Error

doClosureBody :: String -> Value -> Value -> Value
doClosureBody "+" (VNum x) (VNum y) = (VNum $ x + y)
doClosureBody "-" (VNum x) (VNum y) = (VNum $ x - y)
doClosureBody "*" (VNum x) (VNum y) = (VNum $ x * y)
doClosureBody "/" (VNum _) (VNum 0) = Error
doClosureBody "/" (VNum x) (VNum y) = (VNum $ x / y)
doClosureBody ">" (VNum x) (VNum y) = (VBool $ x > y)
doClosureBody ">=" (VNum x) (VNum y) = (VBool $ x >= y)
doClosureBody "=" (VNum x) (VNum y) = (VBool $ x == y)
doClosureBody "++" (VStr x) (VStr y) = (VStr $ x ++ y)
doClosureBody _ _ _ = Error

-- Similar to the above function, though for a special case
negation :: Value -> Value
negation (VBool b) = (VBool $ not b)
negation _ = Error

-- Determines how to evaluate the provided closure and arguments
builtinHelper :: String -> [Expr] -> Env -> Value
builtinHelper "!" (expr:[]) env =  negation (evalDeer expr env)
builtinHelper op (expr:exprs) env = foldl (doClosureBody op) (evalDeer expr env) [(evalDeer ex env) | ex <- exprs]
builtinHelper _ _ _ = Error

-- Given a list of identifiers and expressions, populate them into the first
-- environment provided, and make use of the secone environment to get 
-- values unknown to the first
addBindingsToEnv :: [String] -> [Expr] -> Env -> Env -> Env
addBindingsToEnv [] [] updatedEnv _ = updatedEnv
addBindingsToEnv (binding:bindings) (expr:exprs) updatedEnv externalEnv = addBindingsToEnv bindings exprs (Data.Map.insert binding (evalDeer expr externalEnv) updatedEnv) externalEnv

-------------------------------------------------------------------------------
-- calculateSpreadsheet helpers
-------------------------------------------------------------------------------

-- Return an environment with the appropriate identifier-to-value bindings.
doGetEnvironment :: [Definition] -> Env -> Env
doGetEnvironment [] env = env
doGetEnvironment (def:defs) env = doGetEnvironment defs (getEnvHelper def env)

getEnvHelper :: Definition -> Env -> Env
getEnvHelper (Def name expr) env = Data.Map.insert name (evalDeer expr env) env

-- Return a list of environments, one corresponding to each row in the data.
-- Each environment consists of bindings from the value columns, along with
-- the environment.
buildDataEnvs :: [Column] -> Env -> [Env]
buildDataEnvs [] env = [env] -- Brendan note: Not sure if we need this case, but it doesn't hurt
buildDataEnvs columns env = buildDataEnvsHelper columns (replicate (getLength (head columns)) env)

getLength :: Column -> Int
getLength (ValCol _ values) = length values

buildDataEnvsHelper :: [Column] -> [Env] -> [Env]
buildDataEnvsHelper [] envs = envs
buildDataEnvsHelper (column:columns) envs = buildDataEnvsHelper columns (zipperWrapper column envs)

-- Inserts the values from the column into the respective environemnt to create our
-- environment of rows
zipperWrapper :: Column -> [Env] -> [Env]
zipperWrapper (ValCol id values) envs = zipWith (Data.Map.insert id) values envs

-- Take the list of raw value columns, create our data environments as we go, and calculate the current ComputedCol
-- And append the resulting ValCol to the list, and continue until we run out of ComputedCols
computeSpreadsheetHelper :: [Column] -> [Column] -> Env -> [Column]
computeSpreadsheetHelper valueCols [] _ = valueCols
computeSpreadsheetHelper valueCols ((ComputedCol id formula):computes) env = computeSpreadsheetHelper (valueCols ++ [(ValCol id (doCalculations formula (buildDataEnvs valueCols env)))]) computes env

-------------------------------------------------------------------------------

-- Take in a formula and list of environments, and return
-- a list of the calculated values
doCalculations :: Expr -> [Env] -> [Value]
doCalculations formula envs = [evalDeer formula env | env <- envs]


-------------------------------------------------------------------------------
-- The example from the handout
-------------------------------------------------------------------------------

result = computeSpreadsheet exampleSpreadsheet
