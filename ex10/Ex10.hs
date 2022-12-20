{-|
Module: Ex10
Description: Exercise 10
-}

-- This lists what this module exports. Don't change this!
module Ex10
    (
    mapMaybes,
    composeMaybe,
    foldMaybe, 
    applyBinaryMaybe,
    collectMaybes,
    Pet(..),
    Person(..),
    Robot(..),
    Organization(..),
    robotCompany,
    mapF
    )
where

-------------------------------------------------------------------------------
-- * Task 1: Practice with maybe
-------------------------------------------------------------------------------
-- (a -> b) = function
-- [Maybe a] = list of Just int or Nothing
-- [Maybe b] = return value
mapMaybes :: (a -> b) -> [Maybe a] -> [Maybe b]
mapMaybes _ [] = [] -- base case
mapMaybes op (maybe : maybes) = fmap op maybe : mapMaybes op maybes

-- first (a -> Maybe b)
-- second (b -> Maybe c)
composeMaybe :: (a -> Maybe b) -> (b -> Maybe c) -> (a -> Maybe c)
composeMaybe g f = -- f(g(x))
    \x ->
        case (g x) of 
            Nothing -> Nothing
            Just y -> (f y)

-- Maybe = Just num / Nothing
-- inputs: func accm list 
foldMaybe :: (b -> a -> Maybe b) -> b -> [a] -> Maybe b
foldMaybe func accm [] = 
    case accm of 
        x -> Just x
foldMaybe func accm (x : xs) = 
    case (func accm x) of 
        Nothing -> Nothing
        Just y -> foldMaybe func y xs

applyBinaryMaybe :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
applyBinaryMaybe func arg1 arg2 = 
    case arg1 of 
        Nothing -> Nothing 
        Just x -> case arg2 of 
                    Nothing -> Nothing
                    Just y -> getMaybe (func x y)

getMaybe :: a -> Maybe a
getMaybe a = Just a


helper :: [a] -> a -> [a]
helper lst elem = lst ++ [elem]

collectMaybes :: [Maybe a] -> Maybe [a]
collectMaybes lst = foldl (applyBinaryMaybe helper) (Just []) lst

-------------------------------------------------------------------------------
-- * Task 2: The Eq and Functor Typeclasses
-------------------------------------------------------------------------------

data Pet = Cat String Int -- a cat has a name and an age
         | Dog String Int -- a dog has a name and an age
         | Ants           -- an ant farm is just an ant farm
         deriving Show
instance Eq Pet where
    -- Here, we'll need to make Pet an instance of the Eq type class
    -- function for this datatype. Two pets are equal if they are the
    -- same animal and have the same name.  The two animals don't have
    -- to have the same age. All ants are the same.
    (==) pet1 pet2 = case (pet1, pet2) of
                        (Cat name1 _, Cat name2 _) -> name1 == name2
                        (Dog name3 _, Dog name4 _) -> name3 == name4
                        (Ants, Ants) -> True
                        (_, _) -> False



-- example
fluffy = Cat "Fluffy" 3
myAnts = Ants

-- Here, we'll need to make Organization an instance of the Eq
-- *and* Functor typeclass.
data Person  = Person String Float      -- name, salary
             deriving (Show, Eq)
data Robot   = Robot Int                -- identifier
             deriving (Show, Eq)
data Organization p = Individual p -- organization of one
                    | Team p [Organization p] -- team leader, and list of sub-orgs
                    deriving (Show, Eq)            
instance Functor Organization where
    fmap f (Individual person) = Individual $ f person
    fmap f (Team person lst) = Team (f person) [fmap f p | p <- lst]
    
-- robot organization:
robot1   = Robot 1
robotOrg = Individual robot1

-- example:
owner   = Person "Janet" 100000
cto     = Person "Larry"  90000
cfo     = Person "Mike"   90000
intern  = Person "Sam"    40000
company = Team owner [Team cto [Individual intern],
                      Individual cfo]

-- Use a call to `fmap` to turn the example value `company`
-- into an organization with the same structure, but populated
-- entirely by robots. 
robotize :: Person -> Robot
robotize p = Robot 0

robotCompany = fmap robotize company


-- Write the generalization of the `mapMaybes` function that would work
-- for any functors, not just `Maybe`.
mapF :: Functor f => (a -> b) -> [f a] -> [f b]
mapF _ [] = []
mapF op (x : xs) = fmap op x : mapF op xs

