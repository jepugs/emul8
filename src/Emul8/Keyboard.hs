module Emul8.Keyboard where

import Data.Array
import Emul8.Core

data KeyState = KeyUp | KeyDown
type KeyID = Nybble
type Keyboard = Array KeyID KeyState

initKeyboard :: Keyboard
initKeyboard = array (0x0,0xf) [(i,KeyUp) | i <- [0x0..0xf]]

isUp :: KeyID -> Keyboard -> Bool
isUp i k = case k ! i of
  KeyUp   -> False
  KeyDown -> True

isDown :: KeyID -> Keyboard -> Bool
isDown i = not . isUp i

pressKey :: KeyID -> Keyboard -> Keyboard
pressKey i k = k // [(i,KeyDown)]

releaseKey :: KeyID -> Keyboard -> Keyboard
releaseKey i k = k // [(i,KeyUp)]
