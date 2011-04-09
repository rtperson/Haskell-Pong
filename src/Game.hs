-----------------------------------------------------------------------------
--
-- Module      :  Game
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :  Roger Turnau
-- Stability   :  Experimental
-- Portability :
--
-- | Game logic goes here (paddle location, ball movement, etc.)
--
-----------------------------------------------------------------------------

module Game where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

_LEFT = -2
_RIGHT = 1
_TOP = 1
_BOTTOM = -1

paddleWidth = 0.07
paddleHeight = 0.3 :: GLfloat
ballRadius = 0.035 :: GLfloat

_INITIAL_WIDTH :: GLsizei
_INITIAL_WIDTH = 400

_INITIAL_HEIGHT :: GLsizei
_INITIAL_HEIGHT = 200

_INITIAL_BALL_POSX = -0.8
_INITIAL_BALL_POSY = 0.3
_INITIAL_BALL_DIR = 0.002
_INITIAL_PADDLE_DIR = 0.005 :: GLfloat

type Paddle = (GLfloat, GLfloat, GLfloat)

type BallX = GLfloat
type BallY = GLfloat
type BallDX = GLfloat
type BallDY = GLfloat

data Ball = Ball (BallX, BallY) BallDX BallDY deriving Show

data Game =
    Game { ball :: Ball
         , leftP, rightP :: Paddle
         , points :: (Int, Int)
         , moveFactor :: GLfloat -- the speed at which everything happens, should increase over time
         }

initGame :: Game
initGame =
    Game { ball = Ball (_INITIAL_BALL_POSX,_INITIAL_BALL_POSY)  _INITIAL_BALL_DIR _INITIAL_BALL_DIR
         , leftP = (_LEFT+paddleWidth, 0, 0)
         , rightP = (_RIGHT-2*paddleWidth, 0, 0)
         , points = (0,0)
         , moveFactor = 5
         }

moveBall :: Game -> Ball
moveBall g
    = Ball (x+factor*newXDir, y+factor*newYDir) newXDir newYDir
      where
        (xl,yl,_) = leftP g
        (xr,yr,_) = rightP g
        factor = moveFactor g
        (Ball (x,y) xDir yDir) = ball g
        newXDir
            -- reverse direction if ball hits the left paddle
            |    x - ballRadius <= xl + paddleWidth
              && y + ballRadius >= yl
              && y              <= yl + paddleHeight
              = -xDir
            -- reverse direction if ball hits the right paddle
            |    x + ballRadius >= xr
              && y + ballRadius >= yr
              && y              <= yr + paddleHeight
              = -xDir
            -- stop the ball if it falls off the screen
            | x <= _LEFT - ballRadius = 0
            | x >= _RIGHT + ballRadius = 0
            | otherwise = xDir
        newYDir
            -- bounce off the top and bottom walls
            | y <= _BOTTOM - (ballRadius/4) || y >= _TOP + (ballRadius/4) = -yDir
            -- if x becomes 0, then stop movement along y as well.
            | newXDir == 0 = 0
            | otherwise = yDir

movePaddle :: Paddle -> GLfloat -> Paddle
movePaddle p@(x,y,dir) factor =
    let y1 = y + factor * dir
        newY = min (_TOP-paddleHeight) $ max _BOTTOM y1
    in (x, newY, dir)

