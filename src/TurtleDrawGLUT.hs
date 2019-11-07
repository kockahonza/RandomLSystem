module TurtleDrawGLUT where

import Graphics.UI.GLUT hiding (Line)


data Line = Line (Color3 Float) (Vertex2 Float) (Vertex2 Float)
    deriving (Show)

data Command = Go Float | Turn Float | SetColor (Float, Float, Float) | StartBranch | EndBranch
    deriving (Show)

data Turtle = Turtle (Vertex2 Float) Float (Color3 Float)
    deriving (Show)

-- definitely staying
renderLine :: Line -> IO ()
renderLine (Line col ver1 ver2)= renderPrimitive Lines $ do
        color col
        vertex ver1
        vertex ver2

-- Will be gone
testingLines :: [Line]
testingLines = [
    Line (Color3 255 0 0) (Vertex2 0 0) (Vertex2 1 1),
    Line (Color3 0 255 0) (Vertex2 (-1) 0) (Vertex2 (1) (-1))
          ]

-- will stay but will be renamed and alternated
testing :: IO ()
testing = do
    initialWindowSize $= Size 600 600
    (_progName, _args) <- getArgsAndInitialize
    _window <- createWindow "Hello World"
    displayCallback $= display
    mainLoop

display :: DisplayCallback
display = do 
    clear [ColorBuffer]
    mapM_ renderLine testingLines
    flush
