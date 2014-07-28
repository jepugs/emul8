module Emul8.UI where

import Emul8.Core
import Emul8.Graphics
import Emul8.Keyboard
import Emul8.Machine
import Emul8.Screen

import Control.Applicative
import Data.Array
import Graphics.Rendering.OpenGL.GL.StateVar
import Graphics.Rendering.OpenGL.GL.CoordTrans
import qualified Graphics.UI.GLFW as F


-- Window settings
winSize = Size (8 * fromIntegral scrWidth) (8 * fromIntegral scrHeight)
rgbBits = F.DisplayRGBBits 8 8 8
alphaBits = F.DisplayAlphaBits 0
depthBits = F.DisplayDepthBits 0
stencBits = F.DisplayStencilBits 0
displayBits = [ rgbBits
              , alphaBits
              , depthBits
              , stencBits
              ]
wMode = F.Window

-- OpenGL context settings
glVersionMajor = 2
glVersionMinor = 1

-- the key used to quit the emulator
quitKey = F.ESC

-- initialize
initialize :: IO Bool
initialize = F.initialize

-- cleanup
cleanup :: IO ()
cleanup = F.terminate

-- Open a window with the specified name
openWindow :: String -> IO Bool
openWindow title = do
  F.openWindowHint F.OpenGLVersionMajor glVersionMajor
  F.openWindowHint F.OpenGLVersionMinor glVersionMinor
  b <- F.openWindow winSize displayBits wMode
  F.windowTitle $= title
  return b

-- Emulate a machine.
runMachine :: Machine -> IO ()
runMachine m = do
  F.time $= 0.0
  runWorld 0.0 $ World m 0.0 0.0

-- The data type used to represent the entire emulator state from frame to
-- frame. The fields are, in order:
--  the virtual machine
--  time since an instruction was last executed (seconds)
--  time since the timers were last updated (seconds)
data World = World Machine Double Double

-- the time taken to run each instruction (840 Hz)
instrSpeed = 1 / 840 :: Double
-- the timer update interval (60 Hz)
timerSpeed = 1 / 60 :: Double

-- Emulate a world. The first argument is the time recorded from the last frame.
runWorld :: Double -> World -> IO ()
runWorld t w = do
  F.pollEvents
  qk <- F.getKey quitKey
  if qk == F.Press
    then putStrLn "Quitting..."
    else do
    drawWorld w
    F.swapBuffers
    w' <- handleInput w
    t' <- get F.time
    let world' = emulate (t' - t) w'
    either putStrLn (runWorld t') world'

-- Render the world.
drawWorld :: World -> IO ()
drawWorld (World m _ _) = drawScreen (screen m)

-- mappings of GLFW Keys to Emul8 KeyIDs
keymap :: [(F.Key,KeyID)]
keymap = [ (F.CharKey '1', 0x1)
         , (F.CharKey '2', 0x2)
         , (F.CharKey '3', 0x3)
         , (F.CharKey '4', 0xc)
         , (F.CharKey 'q', 0x4)
         , (F.CharKey 'w', 0x5)
         , (F.CharKey 'e', 0x6)
         , (F.CharKey 'r', 0xd)
         , (F.CharKey 'a', 0x7)
         , (F.CharKey 's', 0x8)
         , (F.CharKey 'd', 0x9)
         , (F.CharKey 'f', 0xe)
         , (F.CharKey 'z', 0xa)
         , (F.CharKey 'x', 0x0)
         , (F.CharKey 'c', 0xb)
         , (F.CharKey 'v', 0xf)
         ]

-- convert a key state
convKS :: F.KeyButtonState -> KeyState
convKS F.Press   = KeyDown
convKS F.Release = KeyUp

-- get the KeyID associated with a Gloss Key
lookupKey :: F.Key -> Maybe KeyID
lookupKey = flip lookup keymap

-- process input
handleInput :: World -> IO World
handleInput (World m ti tt) =
  do F.pollEvents
     keys <- mapM (F.getKey . fst) keymap
     let keys' = map convKS keys
     let kbd'  = kbd m // zip [0x0..0xf] keys'
     let m'    = m { kbd=kbd' }
     case waiting m of
       Nothing -> return $ World m' ti tt
       Just vx -> case findKeyPress (kbd m) kbd' of
         Nothing -> return $ World m { kbd=kbd' } ti tt
         Just k  -> return $ World (setReg vx k m') ti tt

-- Emulate the world for a specified time interval. Takes the elapsed time in
-- seconds as an argument.
emulate :: Double -> World -> Result World
emulate t (World m ti tt)
  | tt' > timerSpeed =
    emInstr timerSpeed wt' >>= emulate (t - timerSpeed)
  | otherwise = emInstr t $ World m ti (tt + t)
  where tt' = t + tt
        mt' = updateTimers m
        wt' = World mt' ti (tt - timerSpeed)

-- Emulate a world without updating timers
emInstr :: Double -> World -> Result World
emInstr t (World m ti tt)
  | ti' > instrSpeed = w' >>= emInstr (t - instrSpeed)
  | otherwise = Right $ World m (ti + t) tt
  where ti' = ti + t
        m' = step m
        w' = (\m -> World m (ti - instrSpeed) tt) <$> m'
