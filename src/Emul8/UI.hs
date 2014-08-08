module Emul8.UI where

import Common.Core
import Common.Load
import Emul8.Graphics
import Emul8.Keyboard
import Emul8.Machine
import Emul8.Screen

import Control.Applicative
import Data.Array
import qualified Graphics.UI.GLFW as F


-- Window settings
winWidth = 16 * fromIntegral scrWidth
winHeight= 16 * fromIntegral scrHeight

-- OpenGL context settings
glVersionMajor = 2
glVersionMinor = 1

-- the key used to quit the emulator
quitKey = F.Key'Escape

-- initialize
initialize :: IO Bool
initialize = F.init

-- cleanup
cleanup :: IO ()
cleanup = F.terminate

-- Open a window with the specified name
openWindow :: String -> IO (Maybe F.Window)
openWindow title = do
  F.windowHint $ F.WindowHint'Resizable False
  F.windowHint $ F.WindowHint'Visible True
  F.windowHint $ F.WindowHint'ContextVersionMajor glVersionMajor
  F.windowHint $ F.WindowHint'ContextVersionMinor glVersionMinor
  w <- F.createWindow winWidth winHeight title Nothing Nothing
  case w of
    Just w' -> do F.setWindowShouldClose w' False
                  F.makeContextCurrent w
                  initViewport
                  return w
    Nothing -> return Nothing

-- Close a window
closeWindow :: F.Window -> IO ()
closeWindow = F.destroyWindow

-- Emulate a machine.
runMachine :: Machine -> F.Window -> Double -> IO ()
runMachine m win is = do
  F.setTime 0.0
  runWorld 0.0 $ World m win 0.0 0.0 is

-- Load a program into a Machine's memory
loadFile :: String -> Addr -> Machine -> IO Machine
loadFile str a m = do
  prog <- readFileProg str
  return $ loadMachine a prog m

-- The data type used to represent the entire emulator state from frame to
-- frame.
data World = World { wMachine :: Machine  -- | The virtual machine
                   , wWindow  :: F.Window -- | The GLFW window
                   , wInstrT  :: Double   -- | Time since an instruction was
                                          -- executed
                   , wTimerT  :: Double   -- | Time since the timers were
                                          -- updated
                   , wInstrS  :: Double   -- | Time to execute each instruction
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
  qk <- F.getKey win quitKey
  if qk == F.KeyState'Pressed
    then putStrLn "Done executing."
    else do
    drawWorld w
    F.swapBuffers win
    w' <- handleInput w
    mt'<- F.getTime
    let t' = case mt' of
          Just x  -> x
          Nothing -> t
    let world' = emulate (t' - t) w'
    either putStrLn (runWorld t') world'
  where win = wWindow w

-- Render the world.
drawWorld :: World -> IO ()
drawWorld w = drawScreen $ screen $ wMachine w

-- mappings of GLFW Keys to Emul8 KeyIDs
keymap :: [(F.Key,KeyID)]
keymap = [ (F.Key'1, 0x1)
         , (F.Key'2, 0x2)
         , (F.Key'3, 0x3)
         , (F.Key'4, 0xc)
         , (F.Key'Q, 0x4)
         , (F.Key'W, 0x5)
         , (F.Key'E, 0x6)
         , (F.Key'R, 0xd)
         , (F.Key'A, 0x7)
         , (F.Key'S, 0x8)
         , (F.Key'D, 0x9)
         , (F.Key'F, 0xe)
         , (F.Key'Z, 0xa)
         , (F.Key'X, 0x0)
         , (F.Key'C, 0xb)
         , (F.Key'V, 0xf)
         ]

-- convert a key state
convKS :: F.KeyState -> KeyState
convKS F.KeyState'Pressed   = KeyDown
convKS F.KeyState'Released  = KeyUp
convKS F.KeyState'Repeating = KeyDown

-- get the KeyID associated with a Gloss Key
lookupKey :: F.Key -> Maybe KeyID
lookupKey = flip lookup keymap

-- process input
handleInput :: World -> IO World
handleInput w =
  do F.pollEvents
     keys <- mapM (F.getKey win . fst) keymap
     let keys' = map convKS keys
     let kbd'  = kbd m // zip (map snd keymap) keys'
     let m'    = m { kbd=kbd' }
     let w'    = w { wMachine=m' }
     case waiting m of
       Nothing -> return w'
       Just vx -> case findKeyPress (kbd m) kbd' of
         Nothing -> return w'
         Just k  -> return w { wMachine=setReg vx k m' { waiting=Nothing } }
  where win = wWindow w
        m  = wMachine w
        ti = wInstrT w
        tt = wTimerT w

-- Emulate the world for a specified time interval. Takes the elapsed time in
-- seconds as an argument.
emulate :: Double -> World -> Result World
emulate t w = emTimer w' >>= emInstr
  where ti = wInstrT w
        tt = wTimerT w
        w' = w { wInstrT=ti + t, wTimerT=tt + t }

-- | Emulate a world, only updating timers
emTimer :: World -> Result World
emTimer w
  | t > timerSpeed = emTimer w'
  | otherwise      = Right w
  where t  = wTimerT w
        m' = updateTimers $ wMachine w
        w' = w { wMachine=m', wTimerT=t - timerSpeed }

-- | Emulate a world without updating timers
emInstr :: World -> Result World
emInstr w
  | t > is    = w' >>= emInstr
  | otherwise = Right w
  where t  = wInstrT w
        is = wInstrS w
        m' = step $ wMachine w
        w' = (\m -> w { wMachine=m, wInstrT=t - is }) <$> m'
