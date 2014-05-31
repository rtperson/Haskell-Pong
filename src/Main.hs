-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :
-- License     :  GPL3
--
-- Maintainer  :  Roger Turnau
-- Stability   :  Experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------


module Main where

import Display
import Game
import Bindings
import Graphics.UI.GLUT as GLUT
import Data.IORef

myInit :: IO ()
myInit = undefined

main :: IO ()
main = do
    (progName, _) <- getArgsAndInitialize
    initialDisplayMode $= [GLUT.DoubleBuffered]
    createWindow progName
    game <- newIORef initGame
    --windowSize $= Size _INITIAL_WIDTH _INITIAL_HEIGHT
    fullScreen
    displayCallback $= display game


    keyboardMouseCallback $= Just (keyboard game)
    reshapeCallback $= Just (reshape game)
    addTimerCallback frameRate $ timer game
    mainLoop

