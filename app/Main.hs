module Main where

import System.Random
import Control.Monad hiding (join)

import LSystem
import TurtleDraw

-- This is the old amazing magick
-- randomNVariation n set = replicateM n (((!!) <$> pure set) <*> randomRIO (0, length set - 1))
randomNVariation :: Int -> [a] -> IO [a]
randomNVariation n set = replicateM n ((set !!) <$> randomRIO (0, length set - 1))

randomVariation :: [a] -> IO [a]
randomVariation set = do
    n <- randomRIO (1, 10)
    randomNVariation n set

randomVarRules :: [a] -> [a] -> IO [(a, [a])]
randomVarRules vars vcs = mapM (\x -> liftMonadFromTuple (return x, randomVariation vcs)) vars
    where
        liftMonadFromTuple = uncurry $ liftM2 (,)

getRandomLSystem :: IO (LSystem Char)
getRandomLSystem = do
    let vars = "AB"
        cons = take 1 (filter (not . (`elem` vars)) ['A'..'Z'])
    axiom <- randomNVariation 1 vars
    rules <- randomVarRules vars (vars ++ cons)
    return (LSystem vars cons axiom rules)


main :: IO ()
main = do
    lsys <- getRandomLSystem
    let finalState = afterNSteps lsys 5
        command = join $ translate [('A', GrabPen blue :#: Go 10), ('B', Turn 30), ('C', GrabPen red :#: Go 20)] finalState
    print lsys
    display $ command
    putStrLn "----------------------------------------------------------------------------------------------------"
    main
