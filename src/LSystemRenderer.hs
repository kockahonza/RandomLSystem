module LSystemRenderer (
    Command (..),
    white, black, red, green, blue,
    showCommands, showLSystem
                      ) where

import Graphics.UI.GLUT hiding (Line)
import Data.IORef
import Data.StateVar
import Data.Maybe

import LSystem


data Command = Go Float | Turn Float | SetColor (Float, Float, Float) | StartBranch | EndBranch
    deriving (Show, Read)

--                       position    angle     color
data Turtle = Turtle (Vertex2 Float) Float (Color3 Float)
    deriving (Show)

data Line = Line (Color3 Float) (Vertex2 Float) (Vertex2 Float)
    deriving (Show)

-- Lets predefine some colors
white, black, red, green, blue :: (Float, Float, Float)
white = (255, 255, 255)
black = (0  , 0  , 0  )
red   = (255, 0  , 0  )
green = (0  , 255, 0  )
blue  = (0  , 0  , 255)

commandsToLines :: [Command] -> [Line]
commandsToLines = fst . turtlePathLines (Turtle (Vertex2 0 0) 0 (Color3 255 255 255))
    where
        turtlePathLines :: Turtle -> [Command] -> ([Line], Maybe [Command])
        turtlePathLines t [] = ([], Nothing)
        turtlePathLines (Turtle tp@(Vertex2 tx ty) ta tc) (Go d:cs) = (Line tc tp newTp : linesAfter, maybeBranchCs)
            where
                aRad = (pi / 180) * ta
                newTp = Vertex2 (tx + d*sin aRad) (ty + d*cos aRad)
                (linesAfter, maybeBranchCs) = turtlePathLines (Turtle newTp ta tc) cs
        turtlePathLines (Turtle tp ta tc) (Turn a:cs) = turtlePathLines (Turtle tp (ta + a) tc) cs
        turtlePathLines (Turtle tp ta tc) (SetColor (r, g, b):cs) = turtlePathLines (Turtle tp ta (Color3 r g b)) cs
        turtlePathLines t (StartBranch:cs) = (inBranchLines ++ afterThisBranchLines, maybeAfterThisBranchCs)
            where
                (inBranchLines, maybeAfterBranchCs) = turtlePathLines t cs
                (afterThisBranchLines, maybeAfterThisBranchCs) = turtlePathLines t (fromJust maybeAfterBranchCs)
        turtlePathLines t (EndBranch:cs) = ([], Just cs)

scaleLine :: Float -> Line -> Line
scaleLine k (Line c v1 v2) = Line c (scaleV k v1) (scaleV k v2)
    where
        scaleV k (Vertex2 vx vy) = Vertex2 (vx * k) (vy * k)

rescaleLines :: [Line] -> [Line]
rescaleLines lines = map (scaleLine k) lines
    where
        maxD = foldr (\(Line c (Vertex2 v1x v1y) (Vertex2 v2x v2y)) p -> maximum $ map abs [p, v1x, v1y, v2x, v2y]) 0 lines
        k = 0.9 * (1 / maxD)

renderLine :: Line -> IO ()
renderLine (Line col ver1 ver2)= renderPrimitive Lines $ do
    color col
    vertex ver1
    vertex ver2

showCommands :: [Command] -> IO ()
showCommands commands = do
    initialWindowSize $= Size 800 800
    (_progName, _args) <- getArgsAndInitialize
    _window <- createWindow "Showing a lsit of commands"
    displayCallback $= showCommandsDisplay commands
    actionOnWindowClose $= ContinueExecution
    mainLoop

showCommandsDisplay :: [Command] -> DisplayCallback
showCommandsDisplay commands = do
    clear [ColorBuffer]
    mapM_ renderLine $ rescaleLines $ commandsToLines commands
    flush

showLSystem :: Ord a => LSystem a Command-> IO ()
showLSystem lsys = do
    initialWindowSize $= Size 800 800
    (_progName, _args) <- getArgsAndInitialize
    _window <- createWindow "Showing a LSystem"

    nIORef <- newIORef (0 :: Int)
    nIORef $= 5

    displayCallback $= showLSystemDisplay lsys nIORef
    keyboardCallback $= Just (showLSystemKeyboard nIORef)
    mouseWheelCallback $= Just (showLSystemMouseWheel nIORef)
    actionOnWindowClose $= ContinueExecution

    mainLoop

showLSystemKeyboard :: IORef Int -> KeyboardCallback
showLSystemKeyboard nIORef c _ = case c of
    '-' -> nIORef $~ max 0 . (+(-1)) >> postRedisplay Nothing
    '+' -> nIORef $~ min 15 . (+1) >> postRedisplay Nothing
    _ -> return ()

showLSystemMouseWheel :: IORef Int -> MouseWheelCallback
showLSystemMouseWheel nIORef _ d _ = nIORef $~ max 1 . (+d) >> postRedisplay Nothing

showLSystemDisplay :: Ord a => LSystem a Command -> IORef Int -> DisplayCallback
showLSystemDisplay lsys nIORef = do
    clear [ColorBuffer]
    n <- get nIORef
    mapM_ renderLine $ rescaleLines $ commandsToLines $ translateAfterNSteps lsys n
    renderString TimesRoman24 (show n)
    flush
