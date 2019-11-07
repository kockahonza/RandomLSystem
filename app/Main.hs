module Main where

import System.Random
import Control.Monad

import LSystem
import TurtleDrawXTurtle
import qualified TurtleDrawGLUT

-- This is the old amazing magick
-- randomNVariation n set = replicateM n (((!!) <$> pure set) <*> randomRIO (0, length set - 1))
randomNVariation :: Int -> [a] -> IO [a]
randomNVariation n set = replicateM n ((set !!) <$> randomRIO (0, length set - 1))

randomVariation :: [a] -> IO [a]
randomVariation set = do
    n <- randomRIO (2, 10)
    randomNVariation n set

randomVarRules :: [a] -> [a] -> IO [(a, [a])]
randomVarRules vars vcs = mapM (\x -> liftMonadFromTuple (return x, randomVariation vcs)) vars
    where
        liftMonadFromTuple = uncurry $ liftM2 (,)

getRandomLSystem :: IO (LSystem Char)
getRandomLSystem = do
    let vars = "AB"
        cons = take 2 (filter (not . (`elem` vars)) ['A'..'Z'])
    axiom <- randomNVariation 1 vars
    rules <- randomVarRules vars (vars ++ cons)
    return (LSystem vars cons axiom rules)

testedMain :: IO ()
testedMain = do
    lsys <- getRandomLSystem
    let finalState = afterNSteps lsys 5
        commandSeq = translate [('A', Go 20), ('B', Go 10), ('C', Turn 90), ('D', Turn (-90))] finalState
    print lsys
    showCommandSeq commandSeq
    testedMain

main :: IO ()
main = do
    TurtleDrawGLUT.testing
