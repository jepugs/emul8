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
winSize = Size (16 * fromIntegral scrWidth) (16 * fromIntegral scrHeight)
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
noResize = True

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
  F.openWindowHint F.NoResize noResize
  F.openWindowHint F.OpenGLVersionMajor glVersionMajor
  F.openWindowHint F.OpenGLVersionMinor glVersionMinor
  b <- F.openWindow winSize displayBits wMode
  F.windowTitle $= title
  initViewport
  return b

-- Emulate a machine.
runMachine :: Machine -> Double -> IO ()
runMachine m is = do
  F.time $= 0.0
  runWorld 0.0 $ World m 0.0 0.0 is

-- The data type used to represent the entire emulator state from frame to
-- frame.
data World = World { wMachine :: Machine -- | The virtual machine
                   , wInstrT  :: Double  -- | Time since an instruction was
                                         -- executed
                   , wTimerT  :: Double  -- | Time since the timers were updated
                   , wInstrS  :: Double  -- | Time to execute each instruction
                   }

-- the time taken to run each instruction (840 Hz)
instrSpeed = 1 / 840 :: Double
-- a faster instrSpeed
fastSpeed = 1 / 1800 :: Double
-- the timer update interval (60 Hz)
timerSpeed = 1 / 60 :: Double

-- Emulate a world. The first argument is the time recorded from the last frame.
runWorld :: Double -> World -> IO ()
runWorld t w = do
  F.pollEvents
  qk <- F.getKey quitKey
  if qk == F.Press
    then putStrLn "Done executing."
    else do
    drawWorld w
    F.swapBuffers
    w' <- handleInput w
    t' <- get F.time
    let world' = emulate (t' - t) w'
    either putStrLn (runWorld t') world'

-- Render the world.
drawWorld :: World -> IO ()
drawWorld w = drawScreen $ screen $ wMachine w

-- mappings of GLFW Keys to Emul8 KeyIDs
keymap :: [(F.Key,KeyID)]
keymap = [ (F.CharKey '1', 0x1)
         , (F.CharKey '2', 0x2)
         , (F.CharKey '3', 0x3)
         , (F.CharKey '4', 0xc)
         , (F.CharKey 'Q', 0x4)
         , (F.CharKey 'W', 0x5)
         , (F.CharKey 'E', 0x6)
         , (F.CharKey 'R', 0xd)
         , (F.CharKey 'A', 0x7)
         , (F.CharKey 'S', 0x8)
         , (F.CharKey 'D', 0x9)
         , (F.CharKey 'F', 0xe)
         , (F.CharKey 'Z', 0xa)
         , (F.CharKey 'X', 0x0)
         , (F.CharKey 'C', 0xb)
         , (F.CharKey 'V', 0xf)
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
handleInput w =
  do F.pollEvents
     keys <- mapM (F.getKey . fst) keymap
     let keys' = map convKS keys
     let kbd'  = kbd m // zip (map snd keymap) keys'
     let m'    = m { kbd=kbd' }
     let w'    = w { wMachine=m' }
     case waiting m of
       Nothing -> return w'
       Just vx -> case findKeyPress (kbd m) kbd' of
         Nothing -> return w'
         Just k  -> return w { wMachine=setReg vx k m' { waiting=Nothing } }
  where m  = wMachine w
        ti = wInstrT w
        tt = wTimerT w

-- Emulate the world for a specified time interval. Takes the elapsed time in
-- seconds as an argument.
emulate :: Double -> World -> Result World
emulate t w
  | tt' > timerSpeed =
    emInstr timerSpeed wt' >>= emulate (t - timerSpeed)
  | otherwise = emInstr t w { wTimerT=tt' }
  where m = wMachine w
        ti = wInstrT w
        tt = wTimerT w
        tt' = t + tt
        mt' = updateTimers m
        wt' = w { wMachine=mt', wTimerT=tt - timerSpeed }

-- Emulate a world without updating timers
emInstr :: Double -> World -> Result World
emInstr t w
  | ti' > is = w' >>= emInstr (t - is)
  | otherwise = Right $ w { wInstrT=ti + t }
  where m = wMachine w
        ti = wInstrT w
        tt = wTimerT w
        is = wInstrS w
        ti' = ti + t
        m' = step m
        w' = (\m -> w { wMachine=m, wInstrT=ti - instrSpeed }) <$> m'
