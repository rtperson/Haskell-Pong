-----------------------------------------------------------------------------
--
-- Module      :  Rendering
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :  Roger Turnau
-- Stability   :  Experimental
-- Portability :
--
-- | Library to simplify rendering shapes onto the screen. Taken from
--   Sven Panitz's OpenGL tutorial
--
-----------------------------------------------------------------------------
module Rendering where

import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL

displayPoints :: [(GLfloat, GLfloat, GLfloat)] -> PrimitiveMode -> IO ()
displayPoints points primitiveShape = do
    renderAs primitiveShape points
    flush

renderAs :: PrimitiveMode -> [(GLfloat, GLfloat, GLfloat)] -> IO ()
renderAs figure ps = renderPrimitive figure $ makeVertices ps

makeVertices :: [(GLfloat, GLfloat, GLfloat)] -> IO ()
makeVertices = mapM_ (\(x,y,z) -> vertex $ Vertex3 x y z)
