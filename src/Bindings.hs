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
-- | Game logic goes here (paddle location, ball movement, etc.)
--
-----------------------------------------------------------------------------
module Bindings where

import Game
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef

keyboard :: IORef Game -> Key -> KeyState -> Modifiers -> Position -> IO ()
keyboard game (Char 'q') Down _ _ = do
    g <- get game
    let (x,y,_) = leftP g
    game $= g{leftP=(x,y,paddleDir Up)}
keyboard game (Char 'q') Up _ _ = do
    g <- get game
    let (x,y,_) = leftP g
    game $= g{leftP=(x,y,0)}
keyboard game (Char 'a') Down _ _ = do
    g <- get game
    let (x,y,_) = leftP g
    game $= g{leftP=(x,y,paddleDir Down)}
keyboard game (Char 'a') Up _ _ = do
    g <- get game
    let (x,y,_) = leftP g
    game $= g{leftP=(x,y,0)}
keyboard game (Char 'o') Down _ _ = do
    g <- get game
    let (x,y,_) = rightP g
    game $= g{rightP=(x,y,paddleDir Up)}
keyboard game (Char 'o') Up _ _ = do
    g <- get game
    let (x,y,_) = rightP g
    game $= g{rightP=(x,y,0)}
keyboard game (Char 'l') Down _ _ = do
    g <- get game
    let (x,y,_) = rightP g
    game $= g{rightP=(x,y,paddleDir Down)}
keyboard game (Char 'l') Up _ _ = do
    g <- get game
    let (x,y,_) = rightP g
    game $= g{rightP=(x,y,0)}

keyboard game (Char '\27') Down _ _ = do
    flush
    exit

keyboard _ _ _ _ _ = return ()

paddleDir :: KeyState -> GLfloat
paddleDir Up   = _INITIAL_PADDLE_DIR
paddleDir Down = -_INITIAL_PADDLE_DIR
