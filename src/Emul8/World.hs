module Emul8.World where

import Common.Core
import Emul8.Machine

import qualified Graphics.UI.GLFW as F


-- | The data type used to represent the entire emulator state from frame to
-- frame.
data World = World { wMachine :: Machine  -- | The virtual machine
                   , wWindow  :: F.Window -- | The GLFW window
                   , wInstrT  :: Double   -- | Time since an instruction was
                                          -- executed
                   , wTimerT  :: Double   -- | Time since the timers were
                                          -- updated
                   , wInstrS  :: Double   -- | Time to execute each instruction
                   }

-- | The default time taken to run each instruction (840 Hz)
instrSpeed = 1 / 840 :: Double
-- | The instrSpeed used for the --fast option
fastSpeed = 1 / 1800 :: Double
-- | The timer update interval (60 Hz)
timerSpeed = 1 / 60 :: Double

-- | Emulate the world for a specified time interval. Takes the elapsed time in
-- seconds as an argument.
emulate :: Double -> World -> Result World
emulate t w @ (World _ _ ti tt _) = emTimer w' >>= emInstr
  where ti = wInstrT w
        tt = wTimerT w
        w' = w { wInstrT=ti + t, wTimerT=tt + t }

-- | Emulate a world, only updating timers.
emTimer :: World -> Result World
emTimer w @ (World m _ _ t _ )
  | t > timerSpeed = emTimer w'
  | otherwise      = Right w
  where m' = updateTimers m
        w' = w { wMachine=m', wTimerT=t - timerSpeed }

-- | Emulate a world without updating timers.
emInstr :: World -> Result World
emInstr w @ (World m _ t _ is)
  | t > is    = w' >>= emInstr
  | otherwise = Right w
  where m' = step m
        w' = (\m -> w { wMachine=m, wInstrT=t - is }) `fmap` m'
