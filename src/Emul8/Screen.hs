module Emul8.Screen where

import Emul8.Core

import Data.Array
import Data.Bits


data Pixel = Empty | Full
type Screen = Array (Byte,Byte) Pixel

flipPx :: Pixel -> Pixel
flipPx Empty = Full
flipPx Full  = Empty

scrWidth = 64 :: Byte
scrHeight= 32 :: Byte
scrMaxX = scrWidth - 1
scrMaxY = scrHeight - 1

initScreen :: Screen
initScreen = array ((0,0),(scrMaxX,scrMaxY))
                    [((x,y),Empty) | x <- [0..scrMaxX],
                                     y <- [0..scrMaxY]]

drawAt :: Byte -> Byte -> [Byte] -> Screen -> Screen
drawAt _ _ []     d = d
drawAt x y (c:cs) d = drawAt x (y + 1) cs d'
  where bs = zip (zip [x..x + 7] (repeat y)) (destructByte c)
        ps = map fst (filter snd bs)
        d' = drawPixels ps d

drawPixels :: [(Byte,Byte)] -> Screen -> Screen
drawPixels ps d = drawPixels ps d'
  where d' = d // [(p,flipPx (d ! p)) | p <- ps]

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
