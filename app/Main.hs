module Main where

import System.Random
import Control.Monad
import System.Console.ArgParser

import LSystem
import LSystemRenderer

import Graphics.UI.GLUT

data Action = RunRandomLSystem | ShowLSystem Bool Bool Int Float deriving (Eq, Show)

actionParser :: IO (CmdLnInterface Action)
actionParser = mkSubParser [
    ("random", setAppDescr (mkDefaultApp (pure RunRandomLSystem) "random")
        "Keep generating and showing random LSystems"),
    ("show", setAppDescr (mkDefaultApp (
                                        ShowLSystem `parsedBy` 
                                        boolFlag "record" `Descr` "Get input in record syntax (the same way it is printed out)." `andBy`
                                        boolFlag "normal" `Descr` "Assume F means go forward, + turn right, - turn left and [] indicates branching" `andBy`
                                        optFlag 5 "steps" `Descr` "The number of steps at which should the program begin." `andBy`
                                        optFlag 90 "angle" `Descr` "The angle to use when the `standard` option is used"
                                       ) "show")
        "Show a specific LSystem. It will be read of standard input")
                             ]

doAction :: Action -> IO ()
doAction RunRandomLSystem = generateAndShowLSystemsForever
doAction (ShowLSystem record standard startN angle) = showLSystemFromStdInput record standard startN angle

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

randomCommand :: IO Command
randomCommand = do
    x <- randomRIO (0, 2) :: IO Int
    case x of
        0 -> do
            d <- randomRIO (1, 2) :: IO Int
            return (Go (10 * fromIntegral d))
        1 -> do
            a <- randomRIO (1, 23) :: IO Int
            return (Turn (15 * fromIntegral a))
        2 -> do
            r <- randomRIO (0, 255) :: IO Int
            g <- randomRIO (0, 255) :: IO Int
            b <- randomRIO (0, 255) :: IO Int
            return (SetColor (fromIntegral r, fromIntegral g, fromIntegral b))

randomCommandsOfLen :: Int -> IO [Command]
randomCommandsOfLen 0 = return []
randomCommandsOfLen n = do
    maybeBranch <- randomRIO (0, 10) :: IO Int
    case maybeBranch of
        0 -> do
            branchLength <- randomRIO (1, n)
            branchCommands <- replicateM branchLength randomCommand
            afterBranchCommands <- randomCommandsOfLen (n - branchLength)
            return $ [StartBranch] ++ branchCommands ++ [EndBranch] ++ afterBranchCommands
        _ -> do
            newRandomCommand <- randomCommand
            remainingCommands <- randomCommandsOfLen (n - 1)
            return $ newRandomCommand : remainingCommands

getRandomLSystem :: Int -> Int -> Int -> Int -> Int -> Int -> IO (LSystem Char Command)
getRandomLSystem vCount cCount aCount ruleMin ruleMax transLen
  | vCount + cCount > 27 = error "There is not enough letters in the alphabet"
  | otherwise = do
    let vars = take vCount ['A'..'Z']
        cons = take cCount (filter (not . (`elem` vars)) ['A'..'Z'])
    axiom <- randomNVariation aCount vars
    rules <- randomVarRulesBounded ruleMin ruleMax vars (vars ++ cons)
    translationRules <- mapM (\x -> randomCommandsOfLen transLen >>= (\c -> return (x, c))) (vars ++ cons)
    return (LSystem vars cons axiom rules translationRules)

generateAndShowLSystemsForever :: IO ()
generateAndShowLSystemsForever = do
    lsys <- getRandomLSystem 2 2 1 5 15 5
    print lsys
    showLSystem 5 lsys
    generateAndShowLSystemsForever

--------------------------------------------------------------------------------
-- ShowLSystem
--------------------------------------------------------------------------------
showLSystemFromStdInput :: Bool -> Bool -> Int -> Float -> IO ()
showLSystemFromStdInput record standard startN angle =
    if record
        then do
            lsys <- readLn :: IO (LSystem Char Command)
            showLSystem startN lsys
        else do
            vars <- getLine
            cons <- getLine
            axiom <- getLine
            rules <- readLn :: IO [(Char, [Char])]
            transRules <- if standard
                             then return [('F', [Go 10]), ('+', [Turn angle]), ('-', [Turn (-angle)]), ('[', [StartBranch]), (']', [EndBranch])]
                             else readLn :: IO [(Char, [Command])]
            let lsys = LSystem vars cons axiom rules transRules
            print lsys
            showLSystem startN lsys

-- Testing

testing :: IO ()
testing = do
    (_progName, _args) <- getArgsAndInitialize
    _window <- createWindow "Hello World"
    displayCallback $= display
    actionOnWindowClose $= ContinueExecution
    mainLoop
 
display :: DisplayCallback
display = do
    clear [ ColorBuffer ]
    renderPrimitive Lines $ do
        color (Color3 (0.5) (0.5) (0.5) :: Color3 GLfloat)
        vertex (Vertex2 0.5 0.7 :: Vertex2 Float)
        vertex (Vertex2 (-0.5) (-0.7) :: Vertex2 Float)
    flush
