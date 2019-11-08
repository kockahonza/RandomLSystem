module TurtleDrawXTurtle (
    Command (..),
    showCommands
                         ) where

import Graphics.X11.Turtle

data Command = Go Double | Turn Double deriving (Show)

getFuncFromCom :: Command -> (Turtle -> IO ())
getFuncFromCom (Go n) = flip forward n
getFuncFromCom (Turn n) = flip right n

showCommands :: [Command] -> IO ()
showCommands commands = do
    f <- openField
    t <- newTurtle f
    hideturtle t
    mapM_ (`getFuncFromCom` t) commands
    waitField f
