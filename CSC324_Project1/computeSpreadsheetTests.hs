import P1(computeSpreadsheet, evalDeer)
import P1Types(Value(..), Column(..), Expr(..), Definition(..), Spreadsheet(..), exampleSpreadsheet)
import qualified Data.Map (empty, fromList, insert)
import Test.QuickCheck (quickCheck, Property, (==>), label)


prop_computeSpreadsheetExample :: Bool
prop_computeSpreadsheetExample = 
    let x = computeSpreadsheet exampleSpreadsheet
    in x == [   ValCol "id" [VNum 1, VNum 2, VNum 3, VNum 4, VNum 5],
                ValCol "name" [VStr "adam",
                            VStr "betty",
                            VStr "clare",
                            VStr "eric",
                            VStr "sam"],
                ValCol "age" [VNum 12, VNum 15, VNum 18, VNum 49, VNum 17],
                ValCol "voter" [(VBool False), (VBool False), (VBool True), (VBool True), (VBool False)],
                ValCol "name2" [VStr "adamadam",
                            VStr "bettybetty",
                            VStr "clareclare",
                            VStr "ericeric",
                            VStr "samsam"],
                ValCol "year-until-100" [VNum 88, VNum 85, VNum 82, VNum 51, VNum 83]]

prop_computeSpreadsheetSimple :: Bool
prop_computeSpreadsheetSimple = 
    let sheet = Spreadsheet [   Def "voting-age" (Literal $ VNum 18),
                                Def "canvote" (Lambda ["x"] 
                                                (Builtin ">=" [Id "x", Id "voting-age"]))]
                                [ValCol "id" [VNum 1, VNum 2, VNum 3, VNum 4, VNum 5],
                                ValCol "name" [VStr "adam",
                                                VStr "betty",
                                                VStr "clare",
                                                VStr "eric",
                                                VStr "sam"],
                                ValCol "age" [VNum 12, VNum 15, VNum 18, VNum 49, VNum 17],
                                ComputedCol "voter" (Apply (Id "canvote") [Id "age"])]
        expected = [   ValCol "id" [VNum 1, VNum 2, VNum 3, VNum 4, VNum 5],
                ValCol "name" [VStr "adam",
                            VStr "betty",
                            VStr "clare",
                            VStr "eric",
                            VStr "sam"],
                ValCol "age" [VNum 12, VNum 15, VNum 18, VNum 49, VNum 17],
                ValCol "voter" [(VBool False), (VBool False), (VBool True), (VBool True), (VBool False)]]
        x = computeSpreadsheet sheet
    in x == expected



-- buitin wrong input type
prop_ErrorStringNum :: Float -> String -> String -> Property
prop_ErrorStringNum arg1 id idval =
  label "Invalid value type" $
    let result =
          (Builtin "++" [Literal $ VNum arg1, (Id id)])
     in evalDeer
          result
          ( Data.Map.insert
              id
              (VStr idval)
              Data.Map.empty
          )
          == Error



prop_testCSPostSpreadsheet :: [String] -> [Float] -> Property
prop_testCSPostSpreadsheet names cgpas =
  (length cgpas > 3 && length names > 3)
    ==> label
      "A spreadsheet indicating if the student made it to CS POSt"
    $ let spreadsheet =
            Spreadsheet
              [ Def "cutoff" (Literal $ VNum 3),
                Def
                  "admitted"
                  ( Lambda
                      ["x"]
                      (Builtin ">=" [Id "x", Id "cutoff"])
                  )
              ]
              [ ValCol "Student Names" [VStr (names !! 0), VStr (names !! 1), VStr (names !! 2)],
                ValCol "CGPA" [VNum (cgpas !! 0), VNum (cgpas !! 1), VNum (cgpas !! 2)],
                ComputedCol "admitted" (Apply (Id "admitted") [Id "CGPA"])
              ]
          result = computeSpreadsheet spreadsheet
       in result
            == [ ValCol "Student Names" [VStr (names !! 0), VStr (names !! 1), VStr (names !! 2)],
                 ValCol "CGPA" [VNum (cgpas !! 0), VNum (cgpas !! 1), VNum (cgpas !! 2)],
                 ValCol "admitted" [VBool ((cgpas !! 0) >= 3), VBool ((cgpas !! 1) >= 3), VBool ((cgpas !! 2) >= 3)]
               ]

prop_testMixedDefs :: Float -> [Float] -> [Float] -> Int -> Property
prop_testMixedDefs y val1 val2 n =
  (n >= 3 && ((length val1 >= n) && (length val2 >= n)))
    ==> label "A spreadsheet with functions and `constant` definitions "
    $ let spreadsheet =
            Spreadsheet
              [ Def "x" (Literal $ VNum (val1 !! 0)),
                Def "y" (Literal $ VNum y),
                Def "f" (Lambda ["x"] (Id "y")),
                Def "g" (Lambda ["y"] (Id "f"))
              ]
              [ ValCol "id" [VNum (val2 !! 1), VNum (val1 !! 1), VNum (val1 !! 2)],
                ComputedCol "fevaled" (Apply (Id "f") [Literal $ VNum (val1 !! 3)]),
                ComputedCol "gevaled" (Apply (Id "f") [Literal $ VNum (val1 !! 4)])
              ]
          result = computeSpreadsheet spreadsheet
       in result
            == [ ValCol "id" [VNum (val2 !! 1), VNum (val1 !! 1), VNum (val1 !! 2)],
                 ValCol "fevaled" [VNum y, VNum y, VNum y],
                 ValCol "gevaled" [VNum y, VNum y, VNum y]
               ]

prop_testNotAllFilled :: [Float] -> Property
prop_testNotAllFilled values =
  (length values >= 3)
    ==> label "A spreadsheet with not all the columns filled, ie not (mxn)"
    $ let spreadsheet =
            Spreadsheet
              []
              [ ValCol "d" [VNum (values !! 0), VNum (values !! 1)],
                ValCol "c" [VNum (values !! 2)]
              ]
          result = computeSpreadsheet spreadsheet
       in result
            == [ ValCol "d" [VNum (values !! 0), VNum (values !! 1)],
                 ValCol "c" [VNum (values !! 2)]
               ]

prop_caculateAddWith1yz :: Float -> Float -> Float -> Property
prop_caculateAddWith1yz x1 y1 z1 =
  label "Lexical Scoping with multiple variables defined (Named Functions)" $
    let expr = (Apply (Id "add1with1") [(Id "y"), (Id "z")])
        env =
          ( Data.Map.insert
              "add1with1"
              ( VClosure
                  ["y", "z"]
                  ( Builtin
                      "+"
                      [ (Id "x"),
                        (Builtin "+" [(Id "y"), (Id "z")])
                      ]
                  )
                  (Data.Map.insert "x" (VNum 1) Data.Map.empty)
              )
              ( Data.Map.insert
                  "x"
                  (VNum x1)
                  ( Data.Map.insert
                      "y"
                      (VNum y1)
                      ( Data.Map.insert
                          "z"
                          (VNum z1)
                          (Data.Map.empty)
                      )
                  )
              )
          )
        result = evalDeer expr env
     in 
        case (result) of
          (VNum returnVal) ->
            ( let x = 1
                  add1with1 y z = x + y + z
                  roundedRet = (round (add1with1 y1 z1))
                  rounded = round (returnVal)
               in rounded == roundedRet
            )

prop_ApplyInvalidFirst :: String -> Float -> Bool -> Property
prop_ApplyInvalidFirst val1 val2 val3 =
  label "First argument of Apply does not evaluate to a closure" $
    let result =
          ( Apply
              (Builtin "+" [Literal $ VNum val2, Literal $ VStr val1])
              [(Literal $ VBool val3)]
          )
     in evalDeer result Data.Map.empty == Error

prop_ApplyInvalidVariableValue :: Float -> String -> String -> Property
prop_ApplyInvalidVariableValue arg1 id idval =
  label "Invalid Id value type" $
    let result =
          (Builtin ">=" [Literal $ VNum arg1, (Id id)])
     in evalDeer
          result
          ( Data.Map.insert
              id
              (VStr idval)
              Data.Map.empty
          )
          == Error

main :: IO ()
main = do
    quickCheck prop_computeSpreadsheetExample
    quickCheck prop_computeSpreadsheetSimple

    quickCheck prop_ErrorStringNum
    -- quickCheck prop_ErrorArgs

    quickCheck prop_testCSPostSpreadsheet
    quickCheck prop_testMixedDefs
    quickCheck prop_testNotAllFilled
    quickCheck prop_caculateAddWith1yz
    quickCheck prop_ApplyInvalidFirst
    quickCheck prop_ApplyInvalidVariableValue