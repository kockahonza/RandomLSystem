module LSystem (
    LSystem (LSystem),
    State, Rules,
    afterNSteps, translate
) where

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
