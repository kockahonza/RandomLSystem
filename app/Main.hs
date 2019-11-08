module Main where

import System.Random
import Control.Monad
import System.Console.ArgParser

import LSystem
import TurtleDrawGLUT

data Action = RunRandomLSystem | ShowLSystem deriving (Eq, Show)

actionParser :: IO (CmdLnInterface Action)
actionParser = mkSubParser [
    ("random", setAppDescr (mkDefaultApp (pure RunRandomLSystem) "random")
        "Keep generating and showing random LSystems"),
    ("show", setAppDescr (mkDefaultApp (pure ShowLSystem) "show")
        "Show a specific LSystem. It will be read of standard input")
                             ]

doAction :: Action -> IO ()
doAction RunRandomLSystem = generateAndShowLSystemsForever
doAction ShowLSystem = showLSystemFromStdInput

main = do
    interface <- actionParser
    runApp interface doAction

--------------------------------------------------------------------------------
-- RunRandomLSystem
--------------------------------------------------------------------------------
-- This is the old amazing magick
-- randomNVariation n set = replicateM n (((!!) <$> pure set) <*> randomRIO (0, length set - 1))
randomNVariation :: Int -> [a] -> IO [a]
randomNVariation n set = replicateM n ((set !!) <$> randomRIO (0, length set - 1))

randomVariationBounded :: Int -> Int -> [a] -> IO [a]
randomVariationBounded lo hi set = do
    n <- randomRIO (lo, hi)
    randomNVariation n set

randomVarRulesBounded :: Int -> Int -> [a] -> [b] -> IO [(a, [b])]
randomVarRulesBounded lo hi vars vcs = mapM (\x -> liftMonadFromTuple (return x, randomVariationBounded lo hi vcs)) vars
    where
        liftMonadFromTuple = uncurry $ liftM2 (,)

getRandomLSystem :: [b] -> IO (LSystem Char b)
getRandomLSystem translationOptions = do
    let vars = "AB"
        cons = take 2 (filter (not . (`elem` vars)) ['A'..'Z'])
    axiom <- randomNVariation 1 vars
    rules <- randomVarRulesBounded 5 15 vars (vars ++ cons)
    translationRules <- randomVarRulesBounded 1 5 (vars ++ cons) translationOptions
    return (LSystem vars cons axiom rules translationRules)

generateAndShowLSystemsForever :: IO ()
generateAndShowLSystemsForever = do
    lsys <- getRandomLSystem [Go 10, Go 20, Turn 90, Turn (-90)]
    print lsys
    showCommands $ translateAfterNSteps lsys 5
    generateAndShowLSystemsForever

--------------------------------------------------------------------------------
-- ShowLSystem
--------------------------------------------------------------------------------
showLSystemFromStdInput :: IO ()
showLSystemFromStdInput = do
    inp <- getLine
    let lsys = read inp :: LSystem Char Command
    showCommands $ translateAfterNSteps lsys 5
