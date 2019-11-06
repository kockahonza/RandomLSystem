module LSystem where

import qualified Data.Map.Strict as Map
import Data.Maybe

type State a = [a]
type Rules a = [(a, [a])]
type RulesMap a = Map.Map a [a]

data LSystem a = LSystem {
    variables :: [a],
    constants :: [a],
    axiom     :: State a,
    rules     :: Rules a
} deriving (Show)

doOneStep :: Ord a => RulesMap a -> State a -> State a
doOneStep rulesMap = concatMap (fromMaybe [] . (rulesMap Map.!?))

afterNSteps :: Ord a => LSystem a -> Int -> State a
afterNSteps (LSystem vars cons axiom rules) n = iterate (doOneStep fullRules) axiom !! n
    where
        fullRules = foldr (\f m -> f m) (Map.fromList rules) [Map.insert x [x] | x <- cons]

translate :: Eq a => [(a, b)] -> State a -> [b]
translate km state = map fromJust $ filter isJust $ map (`lookup` km) state


-- testing
data Atom = P | N | F | G deriving (Show, Eq, Ord)

-- start = [N, F]
-- myrules = Map.fromList [(F, [F, P, F, N, F, N, F, P, F]), (P, [P]), (N, [N])]

start = ['A', 'B']                      :: State Char
myrules = [('A', "AB"), ('B', "A")]     :: Rules Char

myLSystem = LSystem "AB" "" "AB" myrules
