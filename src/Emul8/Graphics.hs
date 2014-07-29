module Emul8.Graphics where

import Emul8.Core
import Emul8.Screen

import Data.Array
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))


-- Initialize the viewport such that 0,0 is the top left corner and 63,31 is the
-- lower right.
initViewport :: IO ()
initViewport = do
  GL.matrixMode $= GL.Projection
  GL.loadIdentity
  GL.ortho 0 (fromIntegral scrWidth) (fromIntegral scrHeight) 0 (-0.5) 0.5
  GL.matrixMode $= GL.Modelview 0


drawScreen :: Screen -> IO ()
drawScreen scr = do
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
    renderPixel
  where x' = fromIntegral x
        y' = fromIntegral y

renderPixel = do
  GL.renderPrimitive GL.Quads $ do
    GL.color $ GL.Color3 0.8 0.8 (0.8 :: GL.GLfloat)
    GL.vertex $ GL.Vertex2 0.0 (0.0 :: GL.GLfloat)
    GL.vertex $ GL.Vertex2 0.0 (1.0 :: GL.GLfloat)
    GL.vertex $ GL.Vertex2 1.0 (1.0 :: GL.GLfloat)
    GL.vertex $ GL.Vertex2 1.0 (0.0 :: GL.GLfloat)
