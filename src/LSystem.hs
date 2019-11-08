module LSystem (
    LSystem (LSystem),
    State, Rules,
    stateAfterNSteps, translateState, translateAfterNSteps
               ) where

import qualified Data.Map.Strict as Map
import Data.Maybe

type State a = [a]
type Rules a b = [(a, [b])]
type RulesMap a b = Map.Map a [b]

data LSystem a b = LSystem {
    variables            :: [a],
    constants            :: [a],
    axiom                :: State a,
    rules                :: Rules a a,
    translationRules     :: Rules a b
                           } deriving (Show, Read)

doOneStep :: Ord a => RulesMap a a -> State a -> State a
doOneStep rulesMap = concatMap (fromMaybe [] . (rulesMap Map.!?))

stateAfterNSteps :: Ord a => LSystem a b -> Int -> State a
stateAfterNSteps (LSystem vars cons axiom rules tr) n = iterate (doOneStep fullRules) axiom !! n
    where
        fullRules = foldr (\f m -> f m) (Map.fromList rules) [Map.insert x [x] | x <- cons]

translateState :: Eq a => LSystem a b -> State a -> [b]
translateState (LSystem v c a r tr) state = concatMap fromJust $ filter isJust $ map (`lookup` tr) state

translateAfterNSteps :: Ord a => LSystem a b -> Int -> [b]
translateAfterNSteps lsys@(LSystem v c a r tr) n = concatMap fromJust $ filter isJust $ map (`lookup` tr) (stateAfterNSteps lsys n)
