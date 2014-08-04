module Main where

import Common.Load
import Dis8.Disassemble

import System.Environment
import System.IO


usageStr =
  "Usage: dis8 FILE [OUTPUT]\n\
  \dis8 is a CHIP-8 program disassembler. FILE is the program to disassemble. If no\n\
  \OUTPUT filename is given, all output is made to stdout.\n\
  \Options:\n\
  \  -h  --help   Show usage information.\n"

showUsage = putStrLn usageStr

-- | Given input and output file names, run the disassembler. If the output file
-- is Nothing, then write output to stdout.
run :: String -> Maybe String -> IO ()
run inf outf = do
  prog <- readFileProg inf
  let output = showProg prog
  case outf of
    Just str -> withFile str WriteMode $ flip hPutStr output
    Nothing  -> putStr output

main = do
  args <- getArgs
  let help = "-h" `elem` args ||
             "--help" `elem` args
  if help
    then showUsage
    else case args of
    (i:[])   -> run i Nothing
    (i:o:[]) -> run i $ Just o
    _        -> showUsage
