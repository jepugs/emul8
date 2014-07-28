module Emul8.Load where

import Emul8.Core
import Emul8.Machine

import Data.ByteString
import System.IO hiding (hGetContents)


loadFile :: String -> Machine -> IO Machine
loadFile str m = do
  prog <- readFileProg str
  return $ loadMachine prog m

readFileProg :: String -> IO Program
readFileProg str = do
  withFile str ReadMode $ \inh -> do
    bs <- hGetContents inh
    return $ unpack bs
