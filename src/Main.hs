module Main where

import Emul8.Core
import Emul8.Font
import Emul8.Graphics
import Emul8.Load
import Emul8.Machine
import Emul8.Screen
import Emul8.UI

import System.Console.GetOpt
import System.Environment
import System.Random


-- | The runtime parameters for the program
data Settings = Settings { sFileName  :: String  -- | The file to load
                         , sSpeed     :: Double  -- | Time taken per instruction
                         , sStartAddr :: Addr    -- | The address to start
                                                 -- execution at.
                         , sProgAddr  :: Addr    -- | The address to load the
                                                 -- program at
                         , sUsage     :: Bool    -- | Whether to show help
                         }

-- | The default settings
defaults = Settings "" instrSpeed memStart memStart False

-- | The header for the usage string.
usageHead =
  "Usage: emul8 [OPTION]... FILE\n\
   \emul8 is a CHIP-8 emulator. FILE is the program to load.\n\
   \Options:"

-- | The string displayed as usage information
usageStr = usageInfo usageHead options

-- | Print the help string.
showUsage = putStrLn usageStr

options :: [OptDescr (Settings -> Settings)]
options = [ Option "h" ["help"]
              (NoArg (\set -> set { sUsage=True }))
              "Show usage information."
          , Option "a" ["start"]
              (ReqArg (\a set -> set { sStartAddr=read a }) "ADDR")
              "Start program execution at ADDR instead of the default 0x200."
          , Option "l" ["load-at"]
              (ReqArg (\a set -> set { sProgAddr=read a }) "ADDR")
              "Load the into memory at ADDR instead of the default 0x200."
          , Option "s" ["speed"]
              (ReqArg (\s set -> set { sSpeed=read s }) "SECONDS")
              "Specify how long it takes to execute each instruction."
          , Option ""  ["fast"]
              (NoArg (\set -> set { sSpeed=fastSpeed }))
              "Run the emulator at fast speed (1/1800 seconds per instruction)."
          ]

-- | Parse arugments into a Settings object.
processArgs :: [String] -> Result Settings
processArgs args = case getOpt Permute options args of
  (o,as, []) -> case as of
    f:[] -> Right $ (foldl (flip id) defaults o) { sFileName=f }
    []   -> Right $ foldl (flip id) defaults o
    _    -> Left $ "multiple filenames\n" ++ usageStr
  (o,_:_,es) -> Left $ "multiple filenames" ++ concat es ++ usageStr
  (o,_  ,es) -> Left $ concat es ++ usageStr

-- | Run the program with the provided settings.
runWithSettings :: Settings -> IO ()
runWithSettings (Settings "" _ _  _  False) =
  putStrLn "no filename"
runWithSettings (Settings f  s sa pa False) = do
  r <- getStdGen
  putStrLn $ "Loading file " ++ f
  m <- loadFile f pa $ (initMachine r) { pc=sa }
  putStrLn "Running. Press <ESC> to quit."
  initialize
  openWindow "Emul8"
  runMachine m instrSpeed
  putStrLn "Exiting..."
  cleanup
runWithSettings (Settings _  _ _  _  True) = showUsage

main = do
  args <- getArgs
  let settings = processArgs args
  either putStrLn runWithSettings settings
