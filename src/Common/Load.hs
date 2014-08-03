module Common.Load where

import Common.Core

import Data.ByteString
import System.IO hiding (hGetContents)


readFileProg :: String -> IO Program
readFileProg str = do
  withFile str ReadMode $ \inh -> do
    bs <- hGetContents inh
    return $ unpack bs
