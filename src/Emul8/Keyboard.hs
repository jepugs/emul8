module Emul8.Keyboard where

import Data.Array
import Emul8.Core

data KeyState = KeyUp | KeyDown
              deriving (Eq,Show)
type KeyID = Nybble
type Keyboard = Array KeyID KeyState

initKeyboard :: Keyboard
initKeyboard = array (0x0,0xf) [(i,KeyUp) | i <- [0x0..0xf]]

isUp :: KeyID -> Keyboard -> Bool
isUp i k = if i > 0xf
           then True
           else case k ! i of
             KeyUp   -> True
             KeyDown -> False

isDown :: KeyID -> Keyboard -> Bool
isDown i = not . isUp i

-- Given two keyboard states, find out if a key was pressed. If multiple keys
-- are pressed, the lowest numerical value is used.
findKeyPress :: Keyboard -> Keyboard -> Maybe KeyID
findKeyPress k k' = f (indices k)
  where f []     = Nothing
        f (i:is) = if k ! i == KeyUp && k' ! i == KeyDown
                   then Just i
                   else f is
