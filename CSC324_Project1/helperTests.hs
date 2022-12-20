import P1(getEnvironment, buildDataEnvs, doCalculations, calculateSpreadsheetHelper)
import P1Types(Expr(..), Value(..), Column(..), Definition(..))
import qualified Data.Map (empty, fromList, insert)
import Test.QuickCheck (quickCheck)

-------------------------------------------------------------------------------
-- getEnvironment tests
-------------------------------------------------------------------------------
prop_getEnvironmentBasic :: Bool
prop_getEnvironmentBasic = 
    let def = (Def "voting-age" (Literal $ VNum 18))
        x = getEnvironment def
    in x == Data.Map.fromList [("voting-age", (VNum 18))]

-------------------------------------------------------------------------------
-- buildDataEnvs tests
-------------------------------------------------------------------------------
prop_buildDataEnvsBasic :: Bool
prop_buildDataEnvsBasic = 
    let cols = [(ValCol "id" [(VNum 1), (VNum 2)]), (ValCol "name" [(VStr "Adam"), (VStr "Betty")])]
        x = (buildDataEnvs cols Data.Map.empty)
    in x == [Data.Map.fromList [("id", (VNum 1)), ("name", (VStr "Adam"))], Data.Map.fromList [("id", (VNum 2)), ("name", (VStr "Betty"))]]

prop_buildDataEnvSingleCol :: Bool
prop_buildDataEnvSingleCol = 
    let cols = [(ValCol "id" [(VNum 1), (VNum 2)])]
        x = (buildDataEnvs cols Data.Map.empty)
    in x == [Data.Map.fromList [("id", (VNum 1))], Data.Map.fromList [("id", (VNum 2))]]

prop_buildDataEnvsEmptyCol :: Bool
prop_buildDataEnvsEmptyCol = 
    let cols = []
        x = (buildDataEnvs cols Data.Map.empty)
    in x == [Data.Map.fromList []]    

-------------------------------------------------------------------------------
-- doCalculations tests
-------------------------------------------------------------------------------
prop_doCalculationsBasic :: Bool
prop_doCalculationsBasic = 
    let canvoteClosure = (VClosure ["x"] (Builtin ">=" [Id "x", Id "voting-age"]) (Data.Map.fromList [("voting-age", (VNum 18))]))
        expr = (Apply (Id "canvote") [Id "age"])
        envs = [Data.Map.fromList [("age", (VNum 55)), ("canvote", canvoteClosure)], Data.Map.fromList [("age", (VNum 17)), ("canvote", canvoteClosure)]]
        x = doCalculations expr envs
    in x == [(VBool True), (VBool False)]

-------------------------------------------------------------------------------
-- calculateSpreadsheetHelper tests
-------------------------------------------------------------------------------
prop_calculateSpreadsheetHelperBasic :: [Column]
prop_calculateSpreadsheetHelperBasic = 
    let valCols = [ValCol "id" [(VNum 1), (VNum 2)], ValCol "name" [VStr "A", VStr "B"], ValCol "age" [VNum 17, VNum 18]]
        compCols = [ComputedCol "voter" (Apply (Id "canvote") [Id "age"])]
        canvoteClosure = (VClosure ["x"] (Builtin ">=" [Id "x", Id "voting-age"]) (Data.Map.fromList [("voting-age", (VNum 18))]))
        defEnv = Data.Map.fromList [("canvote", canvoteClosure), ("voting-age", VNum 18)]
        x = calculateSpreadsheetHelper valCols compCols defEnv
    in x


main :: IO ()
main = do
    quickCheck prop_getEnvironmentBasic
    quickCheck prop_buildDataEnvsBasic
    quickCheck prop_doCalculationsBasic
    quickCheck prop_buildDataEnvSingleCol
    quickCheck prop_buildDataEnvsEmptyCol
    -- quickCheck prop_nameTBDBasic