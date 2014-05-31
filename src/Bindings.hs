-----------------------------------------------------------------------------
--
-- Module      :  Bindings
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :  Roger Turnau
-- Stability   :  Experimental
-- Portability :
--
-- | Keyboard bindings and all other game controls go here.
--
-----------------------------------------------------------------------------
module Bindings where

import Game
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef
import Data.Maybe
import Control.Monad (when)

data PaddleSide = LeftPaddle | RightPaddle deriving Eq

keyboard :: IORef Game -> Key -> KeyState -> Modifiers -> Position -> IO ()
keyboard game key keystate _ _ 
    | key == Char 'q' && keystate == Down = keyHandler (Just (paddleDir Up)) LeftPaddle game
    | key == Char 'q' && keystate == Up   = keyHandler Nothing LeftPaddle game
    | key == Char 'a' && keystate == Down = keyHandler (Just (paddleDir Down)) LeftPaddle game
    | key == Char 'a' && keystate == Up   = keyHandler Nothing LeftPaddle game
    | key == Char 'o' && keystate == Down = keyHandler (Just (paddleDir Up)) RightPaddle game
    | key == Char 'o' && keystate == Up   = keyHandler Nothing RightPaddle game
    | key == Char 'l' && keystate == Down = keyHandler (Just (paddleDir Down)) RightPaddle game
    | key == Char 'l' && keystate == Up   = keyHandler Nothing RightPaddle game   

keyboard game (Char '\32') Down _ _ = do
    g <- get game
    let Ball (x,y) xD yD = ball g
    let xDir
         | x <= _LEFT+3*paddleWidth = _INITIAL_BALL_DIR
         | x >= _RIGHT-3*paddleWidth = -_INITIAL_BALL_DIR
         | otherwise = xD
    when (xD == 0) $
        game $= g{ball = Ball (x+4*xDir,y) xDir _INITIAL_BALL_DIR}

keyboard game (Char '\27') Down _ _ = do
    flush
    exit

keyboard _ _ _ _ _ = return ()

keyHandler :: Maybe GLfloat -> PaddleSide -> IORef Game-> IO ()
keyHandler func paddle game
    | isJust func && paddle == LeftPaddle  = do
        g <- get game
        let (x,y,_) = leftP g
        game $= g{leftP=(x,y,fromJust func)}
    | isJust func && paddle == RightPaddle = do 
        g <- get game
        let (x,y,_) = rightP g
        game $= g{rightP=(x,y,fromJust func)}
    | isNothing func && paddle == LeftPaddle = do
        g <- get game
        let (x,y,_) = leftP g
        game $= g{leftP=(x,y,0)}
    | otherwise = do
        g <- get game
        let (x,y,_) = rightP g
        game $= g{rightP=(x,y,0)}

paddleDir :: KeyState -> GLfloat
paddleDir Up   = _INITIAL_PADDLE_DIR
paddleDir Down = -_INITIAL_PADDLE_DIR
