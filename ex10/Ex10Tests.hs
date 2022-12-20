module Ex10Tests where

import Data.Maybe (isNothing)
import Ex10
  ( Organization (..),
    Person (..),
    applyBinaryMaybe,
    collectMaybes,
    composeMaybe,
    foldMaybe,
    mapF,
    mapMaybes,
  )
import Test.QuickCheck (Property, label, quickCheck)

fa :: Int -> Int
fa a = a + 1

-- tests
prop_testmapMaybeEmpty :: Property
prop_testmapMaybeEmpty =
  label "Test empty list" $
    let lst = []
        result = mapMaybes (+ 1) lst
     in null result

prop_testmapMaybeOneNothing :: Property
prop_testmapMaybeOneNothing =
  label "Test with Nothing" $
    let lst = [Nothing]
        result = mapMaybes (+ 1) lst
     in result == [Nothing]

prop_testmapMaybeOneJust :: Property
prop_testmapMaybeOneJust =
  label "Test with Just" $
    let lst = [Just 5]
        result = mapMaybes (+ 0) lst
     in result == [Just 5]

prop_testmapMaybeMultiJusts :: Property
prop_testmapMaybeMultiJusts =
  label "Test with Justs" $
    let lst = [Just 5, Just 0, Just 10]
        result = mapMaybes (/ 5) lst
     in result == [Just 1, Just 0, Just 2]

prop_testmapMaybeMulitMix :: Property
prop_testmapMaybeMulitMix =
  label "Test with Justs and Nothing" $
    let lst = [Just 5, Just 0, Just 10, Nothing, Nothing, Just 4]
        result = mapMaybes (/ 5) lst
     in result == [Just 1, Just 0, Just 2, Nothing, Nothing, Just 0.8]

-- composeMaybe
f :: Float -> Maybe Bool
f 0 = Nothing
f 4 = Just True
f _ = Just False

g :: Bool -> Maybe Int
g True = Just 1
g _ = Nothing

prop_testComposeMaybeFG :: Property
prop_testComposeMaybeFG =
  label "Tests composeMaybe with functions f and g" $
    let call = composeMaybe f g 4 -- g(f(4))
     in call == Just 1

h :: Int -> Maybe Int
h 0 = Nothing
h a = Just (a + 1)

k :: Int -> Maybe Bool
k 0 = Nothing
k a = Just (a < 2)

prop_testComposeMaybeHK :: Property
prop_testComposeMaybeHK =
  label "Tests composeMaybe with functions h and k" $
    let call = composeMaybe h k 0
     in isNothing call

j :: Int -> Maybe Int
j 0 = Nothing
j a = Just (a + 1)

m :: Int -> Maybe Int
m 0 = Nothing
m a = Just 5

prop_testComposeMaybeJM :: Property
prop_testComposeMaybeJM =
  label "Tests composeMaybe with functions h and k" $
    let call = composeMaybe m j 5
     in call == Just 6




foldMaybeHelper :: Float -> Float -> Maybe Float
foldMaybeHelper _ 0.0 = Nothing
foldMaybeHelper a b = Just (a + b)

prop_testFoldMaybe0 :: Property
prop_testFoldMaybe0 =
  label "Test foldMaybe with a list with empty list with foldMaybeHelper" $
    let lst = []
        acc = 0
        result = foldMaybe foldMaybeHelper acc lst
     in result == Just 0

prop_testFoldMaybe1 :: Property
prop_testFoldMaybe1 =
  label "Test foldMaybe with a list with one element foldMaybeHelper" $
    let lst = [1]
        acc = 0
        result = foldMaybe foldMaybeHelper acc lst
     in result == Just 1

prop_testFoldMaybe3 :: Property
prop_testFoldMaybe3 =
  label "Test foldMaybe with a list of three elements foldMaybeHelper" $
    let lst = [1, 2, 3]
        acc = 2
        result = foldMaybe foldMaybeHelper acc lst
     in result == Just 8

-- applyBinaryMaybe
binaryHelperadd :: Float -> Float -> Float
binaryHelperadd a b = a + b

-- tests
prop_testApplyMaybeAdd :: Property
prop_testApplyMaybeAdd =
  label "Tests applyBinaryMaybe with addition helper" $
    let arg1 = Just 4
        arg2 = Just 5
        result = applyBinaryMaybe binaryHelperadd arg1 arg2
     in result == Just 9

binaryHelpersub :: Float -> Float -> Float
binaryHelpersub a b = a - b

prop_testApplyMaybeSub :: Property
prop_testApplyMaybeSub =
  label "Tests applyBinaryMaybe with subtraction helper" $
    let arg1 = Just 5
        arg2 = Just 3
        result = applyBinaryMaybe binaryHelpersub arg1 arg2
     in result == Just 2

-- collectMaybes testing
prop_testCollectMaybesAllJusts :: Property
prop_testCollectMaybesAllJusts =
  label "Tests collectMaybes with All Justs" $
    let lst = [Just 2, Just 3, Just 10]
        result = collectMaybes lst
     in result == Just [2, 3, 10]

prop_testCollectMaybesOneNothing :: Property
prop_testCollectMaybesOneNothing =
  label "Tests collectMaybes with one Nothing" $
    let lst = [Just 2, Just 3, Nothing]
        result = collectMaybes lst
     in isNothing result

prop_testCollectMaybesAllNothing :: Property
prop_testCollectMaybesAllNothing =
  label "Tests collectMaybes with all Nothing" $
    let lst = [Nothing, Nothing, Nothing]
        result = collectMaybes lst
     in isNothing result

-- testing fmap for Organization
owner :: Person
owner = Person "Janet" 100000

cto :: Person
cto = Person "Larry" 90000

cfo :: Person
cfo = Person "Mike" 90000

intern :: Person
intern = Person "Sam" 40000

company :: Organization Person
company =
  Team
    owner
    [ Team cto [Individual intern],
      Individual cfo
    ]

incrementSalary :: Person -> Person
incrementSalary (Person name salary) =
  Person name (salary + 1)

prop_testMapExample :: Property
prop_testMapExample =
  label "Tests fmap with incrementSalary" $
    let fmap' = fmap incrementSalary company
     in fmap'
          == Team (Person "Janet" 100001.0) [Team (Person "Larry" 90001.0) [Individual (Person "Sam" 40001.0)], Individual (Person "Mike" 90001.0)]

-- testing for mapF
prop_testmapFExample :: Property
prop_testmapFExample =
  label "Tests mapF with incrementSalary" $
    let fmap' = mapF incrementSalary [company]
     in fmap'
          == [ Team
                 (Person "Janet" 100001.0)
                 [ Team
                     (Person "Larry" 90001.0)
                     [ Individual
                         (Person "Sam" 40001.0)
                     ],
                   Individual (Person "Mike" 90001.0)
                 ]
             ]

-- list of list
prop_testmapFInts :: Property
prop_testmapFInts =
  label "Tests mapF with all Integers" $
    let lst = [[1], [2], [5]]
        call = mapF (+ 1) lst
     in call == [[2], [3], [6]]

----------------------------------------------------------------------
-- Very simple example
----------------------------------------------------------------------
data MyInt a
  = Zero
  | MyInt a
  deriving (Show, Eq)

instance Functor MyInt where
  fmap _ Zero = Zero
  fmap f (MyInt a) = MyInt (f a)

myIntHelper :: Int -> Bool
myIntHelper a = a > 5

prop_testmapFMyInt :: Property
prop_testmapFMyInt =
  label "Tests mapF with MyInt datatype" $
    let lst = [Zero, MyInt 3, MyInt 20, Zero, MyInt 3]
        re = mapF myIntHelper lst
     in re == [Zero, MyInt False, MyInt True, Zero, MyInt False]

main :: IO ()
main = do
  quickCheck prop_testmapMaybeEmpty
  quickCheck prop_testmapMaybeOneNothing
  quickCheck prop_testmapMaybeOneJust
  quickCheck prop_testmapMaybeMultiJusts
  quickCheck prop_testmapMaybeMulitMix
  quickCheck prop_testComposeMaybeFG
  quickCheck prop_testComposeMaybeHK
  quickCheck prop_testComposeMaybeJM
  quickCheck prop_testFoldMaybe0
  quickCheck prop_testFoldMaybe1
  quickCheck prop_testFoldMaybe3
  quickCheck prop_testApplyMaybeAdd
  quickCheck prop_testApplyMaybeSub

  quickCheck prop_testCollectMaybesAllJusts
  quickCheck prop_testCollectMaybesOneNothing
  quickCheck prop_testCollectMaybesAllNothing
  quickCheck prop_testMapExample
  quickCheck prop_testmapFExample
  quickCheck prop_testmapFMyInt
  quickCheck prop_testmapFInts