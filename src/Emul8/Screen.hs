module Emul8.Screen where

import Emul8.Core

import Data.Array
import Data.Bits


-- A single pixel on the screen
data Pixel = Empty | Full
           deriving Eq

type Screen = Array (Byte,Byte) Pixel


-- Toggle a pixel
flipPx :: Pixel -> Pixel
flipPx Empty = Full
flipPx Full  = Empty

-- The screen width
scrWidth = 64 :: Byte
-- The screen height
scrHeight= 32 :: Byte
-- The largest possible x coordinate
scrMaxX = scrWidth - 1
-- The largest possible y coordinate
scrMaxY = scrHeight - 1

-- The initial screen, with all empty pixels
initScreen :: Screen
initScreen = array ((0,0),(scrMaxX,scrMaxY))
                    [((x,y),Empty) | x <- [0..scrMaxX],
                                     y <- [0..scrMaxY]]


-- | Given x and y coordinates, a list of bytes (representing a sprite), and a
-- pixel wrapping mode, draw a sprite. The returned Bool is whether or not a
-- collision happened.
drawAt :: Byte -> Byte -> [Byte] -> Bool -> Screen -> (Screen,Bool)
drawAt _ _ []     _ d = (d,False)
drawAt x y (c:cs) w d = if col
                        then (fst scr',True)
                        else scr'
  where bs = zip (zip [x..x + 7] (repeat y)) (destructByte c)
        ps = map fst (filter snd bs)
        (d',col) = drawPixels ps w d
        scr' = drawAt x (y + 1) cs w d'

-- | Toggle pixels at the specified coordinates with optional pixel wrapping.
drawPixels :: [(Byte,Byte)] -> Bool -> Screen -> (Screen,Bool)
drawPixels ps w d = (d',col)
  where ps' = if w
              then map mf ps  -- ensure that pixels wrap rather than causing an
                              -- array out of bounds error
              else filter ff ps  -- truncate out-of-bounds pixels
        ff (x,y) = x >= 0       &&
                   y >= 0       &&
                   x <= scrMaxX &&
                   y <= scrMaxY
        mf (x,y) = (x `mod` scrWidth,y `mod` scrHeight)
        col = any (== Full) $ map (d !) ps'
        d' = d // [(p,flipPx (d ! p)) | p <- ps']

-- Destruct a byte into a list of 8 booleans, with True representing a high bit
-- and False representing a low bit. The resulting list is ordered from MSB down
destructByte :: Byte -> [Bool]
destructByte c = [b0,b1,b2,b3,b4,b5,b6,b7]
  where b0 = c .&. 0x80 == 0x80
        b1 = c .&. 0x40 == 0x40
        b2 = c .&. 0x20 == 0x20
        b3 = c .&. 0x10 == 0x10
        b4 = c .&. 0x08 == 0x08
        b5 = c .&. 0x04 == 0x04
        b6 = c .&. 0x02 == 0x02
        b7 = c .&. 0x01 == 0x01
