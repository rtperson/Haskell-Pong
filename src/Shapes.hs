-----------------------------------------------------------------------------
--
-- Module      :  Shapes
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :  Roger Turnau
-- Stability   :  Experimental
-- Portability :
--
-- | Renders all shapes necessary for Pong
--
-----------------------------------------------------------------------------
module Shapes where

import Rendering
import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL

{- Squares and Rectangles -}
rectangle :: GLfloat -> GLfloat -> IO()
rectangle length width =
    displayPoints [(l,w,0), (l,-w,0),(-l,-w,0),(-l,w,0)] Quads
    where
        l = length / 2
        w = width / 2

square :: GLfloat -> IO()
square width = rectangle width width

{- Circles -}

circlePoints :: GLfloat -> GLfloat -> [(GLfloat, GLfloat, GLfloat)]
circlePoints radius number
    = [let alpha = (2*pi) * i / number
       in (radius * sin alpha, radius * cos alpha, 0)
      | i <- [1,2..number]]

circle :: GLfloat -> [(GLfloat, GLfloat, GLfloat)]
circle radius = circlePoints radius 100

renderCircle :: GLfloat -> IO ()
renderCircle r = displayPoints (circle r) LineLoop

fillCircle :: GLfloat -> IO ()
fillCircle r = displayPoints (circle r) Polygon

