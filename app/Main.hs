module Main where

import System.Random
import Control.Monad
import System.Console.ArgParser

import LSystem
import TurtleDrawGLUT

data Action = RunRandomLSystem | ShowLSystem Bool Float deriving (Eq, Show)

actionParser :: IO (CmdLnInterface Action)
actionParser = mkSubParser [
    ("random", setAppDescr (mkDefaultApp (pure RunRandomLSystem) "random")
        "Keep generating and showing random LSystems"),
    ("show", setAppDescr (mkDefaultApp (
                                        ShowLSystem `parsedBy` 
                                        boolFlag "standard" `Descr` "Assume F means go forward, P turn right and M turn left" `andBy`
                                        optFlag 90 "angle" `Descr` "The angle to use when the `standard` option is used"
                                       ) "show")
        "Show a specific LSystem. It will be read of standard input")
                             ]

doAction :: Action -> IO ()
doAction RunRandomLSystem = generateAndShowLSystemsForever
doAction (ShowLSystem standard angle) = showLSystemFromStdInput standard angle

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

randomVarRulesBounded :: Int -> Int -> [a] -> [b] -> IO (Rules a b)
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
    lsys <- getRandomLSystem [Go 10, Go 10, Go 10, Turn 15, Turn (-15), SetColor red, SetColor green, SetColor blue]
    print lsys
    showLSystem lsys
    generateAndShowLSystemsForever

--------------------------------------------------------------------------------
-- ShowLSystem
--------------------------------------------------------------------------------
showLSystemFromStdInput :: Bool -> Float -> IO ()
showLSystemFromStdInput standard angle = do
    vars <- getLine
    cons <- getLine
    axiom <- getLine
    rules <- readLn :: IO [(Char, [Char])]
    transRules <- if standard then return [('F', [Go 10]), ('P', [Turn angle]), ('M', [Turn (-angle)])] else readLn :: IO [(Char, [Command])]
    let lsys = LSystem vars cons axiom rules transRules
    showLSystem lsys
