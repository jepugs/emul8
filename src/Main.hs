module Main where

import Emul8.Core
import Emul8.Font
import Emul8.Graphics
import Emul8.Load
import Emul8.Machine
import Emul8.Screen
import Emul8.UI

import System.Environment
import System.Random


main = do
  args <- getArgs
  if length args /= 1
  then do
    putStrLn "Usage: emul8 FILE"
  else do
    let file = head args
    initialize
    stdGen <- getStdGen
    putStrLn $ "Loading file " ++ file
    m <- loadFile file $ initMachine stdGen
    putStrLn "Running. Press <ESC> to quit."
    openWindow "Emul8"
    runMachine m
    putStrLn "Done executing."
    cleanup
