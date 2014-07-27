module Emul8.Graphics where

import Emul8.Core
import Emul8.Screen

import Data.Array
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))


drawScreen :: Screen -> IO ()
drawScreen scr = do
  GL.matrixMode $= GL.Projection
  GL.loadIdentity
  GL.ortho 0 (fromIntegral scrWidth) (fromIntegral scrHeight) 0 0 0
  GL.matrixMode $= GL.Modelview 0
  GL.clear [ GL.ColorBuffer ]
  renderPixels (assocs scr)

renderPixels :: [((Byte,Byte),Pixel)] -> IO ()
renderPixels = mapM_ pixelAt . map fst . filter ff
  where ff (_,p) = case p of
          Empty -> False
          Full  -> True

pixelAt :: Integral a => (a,a) -> IO ()
pixelAt (x,y) = do
  GL.preservingMatrix $ do
    GL.translate $ GL.Vector3 x' y' (0.0 :: GL.GLfloat)
  where x' = fromIntegral x
        y' = fromIntegral y

renderPixel = do
  GL.renderPrimitive GL.Quads $ do
    GL.color $ GL.Color3 0.4 0.4 (0.4 :: GL.GLfloat)
    GL.vertex $ GL.Vertex2 0.0 (0.0 :: GL.GLfloat)
    GL.vertex $ GL.Vertex2 0.0 (1.0 :: GL.GLfloat)
    GL.vertex $ GL.Vertex2 1.0 (1.0 :: GL.GLfloat)
    GL.vertex $ GL.Vertex2 1.0 (0.0 :: GL.GLfloat)
