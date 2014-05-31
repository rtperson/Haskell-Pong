-----------------------------------------------------------------------------
--
-- Module      :  Display
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :  Roger Turnau
-- Stability   :  Experimental
-- Portability :
--
-- | All display logic goes here. Is this overkill for a game of Pong? Absolutely.
--   But it helps us when the programs grow larger (which I'm hoping they will...)
--
-----------------------------------------------------------------------------
module Display where

import Graphics.UI.GLUT  (swapBuffers, postRedisplay, addTimerCallback, Timeout)
import Graphics.Rendering.OpenGL 
import Data.IORef
import Game
import Shapes

displayPaddle :: (GLfloat, GLfloat, t) -> IO ()
displayPaddle (x,y,_) = preservingMatrix $ do
    translate $ Vector3 (paddleWidth/2) (paddleHeight/2) 0
    displayAt (x,y) $ rectangle paddleWidth paddleHeight

displayAt :: (GLfloat, GLfloat) -> IO a -> IO a
displayAt (x,y) displayMe = preservingMatrix $ do
    translate $ Vector3 x y (0::GLfloat)
    displayMe

display :: IORef Game -> IO()
display game = do
    clear[ColorBuffer]
    g <- get game
    let (Ball pos xDir yDir) = ball g
    displayAt pos $ fillCircle ballRadius
    displayPaddle $ leftP g
    displayPaddle $ rightP g
    swapBuffers

timer :: IORef Game -> IO ()
timer game = do
    addTimerCallback frameRate $ timer game
    g <- get game
    let fac = moveFactor g
    game
        $= g{ ball = moveBall g
            , leftP = movePaddle (leftP g) fac
            , rightP = movePaddle (rightP g) fac
            }
    postRedisplay Nothing


frameRate :: Timeout
frameRate = 1000 `div` 60

reshape :: t -> Size -> IO ()
reshape game s@(Size w h) = do
    viewport $= (Position 0 0, s)
    matrixMode $= Projection
    loadIdentity
    ortho (-2.0) 1.0 (-1.0) 1.0 (-1.0) 1.0
    matrixMode $= Modelview 0

