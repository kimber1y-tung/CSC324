import P1(evalDeer)
import P1Types(Expr(..), Value(..))
import qualified Data.Map (empty, fromList, insert)
import Test.QuickCheck (quickCheck)

prop_evalLiteral :: Bool
prop_evalLiteral = 
    let x = (evalDeer (Literal $ VNum 4) Data.Map.empty)
    in x == (VNum 4)

prop_evalEnvLookup :: Bool
prop_evalEnvLookup = 
    let x = (evalDeer (Id "a") (Data.Map.fromList [("a", (VNum 5))]))
    in x == (VNum 5)

prop_evalApplyGreaterThan :: Bool
prop_evalApplyGreaterThan = 
    let env = Data.Map.fromList [("age", (VNum 66)), 
                                 ("canvote", (VClosure ["x"] 
                                                (Builtin ">=" [Id "x", Id "voting-age"]) 
                                                (Data.Map.fromList [("voting-age", (VNum 18))])))]
        x = (evalDeer (Apply (Id "canvote") [Id "age"]) env)
        in x == (VBool True)

prop_evalApply :: Bool
prop_evalApply = 
    let env = Data.Map.fromList [("x", (VNum 3)), ("y", (VNum 2)), ("adder", (VClosure ["a", "b"] (Builtin "+" [Id "a", Id "b"]) Data.Map.empty))]
        x = (evalDeer (Apply (Id "adder") [Id "x", Id "y"]) env)
    in x == (VNum 5)

prop_evalApplyClosureEnv :: Bool
prop_evalApplyClosureEnv = 
    let env = Data.Map.fromList [("x", (VNum 3)), ("add3", (VClosure ["a"] (Builtin "+" [Id "a", Id "inEnv"]) (Data.Map.fromList [("inEnv", (VNum 3))])))]
        x = (evalDeer (Apply (Id "add3") [Id "x"]) env)
    in x == (VNum 6)

prop_evalErronousLambdaDef :: Bool
prop_evalErronousLambdaDef = 
    let lambda = (Lambda [] (Builtin "+" [Literal $ VNum 3, Literal $ VStr "hello"]))
        x = (evalDeer lambda Data.Map.empty)
    in x == (VClosure [] (Builtin "+" [Literal $ VNum 3, Literal $ VStr "hello"]) Data.Map.empty)

-------------------------------------------------------------------------------
-- Tests to check Error cases
-------------------------------------------------------------------------------
prop_evalWrongInput :: Bool
prop_evalWrongInput = 
    let env = Data.Map.fromList [("x", (VStr "hello")), ("adder", (VClosure ["a"] (Builtin "+" [Id "a", (Literal $ VNum 3)]) Data.Map.empty))]
        x = (evalDeer (Apply (Id "adder") [Id "x"]) env)
    in x == Error

prop_evalDivideByZero :: Bool
prop_evalDivideByZero = 
    let divide = (VClosure ["x"] (Builtin "/" [Id "x", (Literal $ VNum 0)]) Data.Map.empty)
        env = Data.Map.fromList [("num", (VNum 13)), ("div", divide)]
        x = (evalDeer (Apply (Id "div") [Id "num"]) env)
    in x == Error

prop_evalFunctionTooManyArgs :: Bool
prop_evalFunctionTooManyArgs = 
    let func = ("adder", (VClosure ["a"] (Builtin "+" [Id "a", (Literal $ VNum 3)]) Data.Map.empty))
        env = Data.Map.fromList [("val1", (VNum 3)), ("val2", (VNum 2)), func]
        x = (evalDeer (Apply (Id "adder") [Id "val1", Id "val2"]) env)
    in x == Error

prop_evalFunctionTooFewArgs :: Bool
prop_evalFunctionTooFewArgs = 
    let func = ("adder", (VClosure ["a"] (Builtin "+" [Id "a", Id "b"]) Data.Map.empty))
        env = Data.Map.fromList [("val", (VNum 3)), ("b", (VNum 2)), func]
        x = (evalDeer (Apply (Id "adder") [Id "val"]) env)
    in x == Error

prop_evalUnkownIdentifier :: Bool
prop_evalUnkownIdentifier = 
    let func = ("adder", (VClosure ["a"] (Builtin "+" [Id "a", Literal $ VNum 3]) Data.Map.empty))
        env = Data.Map.fromList [func, ("rightThing", (VNum 2))]
        x = (evalDeer (Apply (Id "adder") [Id "wrongThing"]) env)
    in x == Error

main :: IO ()
main = do
    quickCheck prop_evalLiteral
    quickCheck prop_evalEnvLookup
    quickCheck prop_evalApply
    quickCheck prop_evalApplyClosureEnv
    quickCheck prop_evalApplyGreaterThan
    quickCheck prop_evalErronousLambdaDef
    -- Error tests
    quickCheck prop_evalWrongInput
    quickCheck prop_evalDivideByZero
    quickCheck prop_evalFunctionTooManyArgs
    quickCheck prop_evalFunctionTooFewArgs
    quickCheck prop_evalUnkownIdentifier