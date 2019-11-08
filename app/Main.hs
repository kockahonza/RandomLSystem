module Main where

import System.Random
import Control.Monad

import LSystem
import TurtleDrawGLUT

-- This is the old amazing magick
-- randomNVariation n set = replicateM n (((!!) <$> pure set) <*> randomRIO (0, length set - 1))
randomNVariation :: Int -> [a] -> IO [a]
randomNVariation n set = replicateM n ((set !!) <$> randomRIO (0, length set - 1))

randomVariation :: [a] -> IO [a]
randomVariation set = do
    n <- randomRIO (2, 12)
    randomNVariation n set

randomVarRules :: [a] -> [b] -> IO [(a, [b])]
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

main :: IO ()
main = do
    lsys <- getRandomLSystem
    translationRules <- randomVarRules "ABCD" [Go 10, Go 20, Turn 90, Turn (-90)]
    let finalState = afterNSteps lsys 5
        commands = translate translationRules finalState
    showCommands commands
    print lsys
    main
