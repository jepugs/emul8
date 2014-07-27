module Main where

import Emul8.Core
import Emul8.Machine
import Emul8.UI

import System.Random


main = do
  initialize
  openWindow "Emul8"
  stdGen <- getStdGen
  runMachine $ initMachine stdGen
  cleanup
