module Common.Core where

import Data.Bits
import qualified Data.Word as W (Word8,Word16)


-- generic type used for error handling
type Result a = Either String a

type Nybble = W.Word8
type Byte   = W.Word8
type Word   = W.Word16

-- Types used within the VM
type Vx   = Nybble    -- register identifier
type Vy   = Nybble    -- register identifier
type Addr = W.Word16  -- address (actually 12 bits)
type Op   = Word      -- opcode
type Program = [Byte] -- Program

-- Masks used to ensure that values fall within the proper range
nybMask  =   0x0f :: Nybble
addrMask = 0x0fff :: Addr
byteMask =   0xff :: Byte

ensureNyb  = (nybMask  .&.)
ensureAddr = (addrMask .&.)

--- Memory max and min address values
memMin = 0x000 :: Addr
memMax = 0xfff :: Addr

validAddr :: Addr -> Bool
validAddr a = a <= memMax && a >= memMin


-- get different hexadecimal digits from a word
wordDigit1 :: Word -> Nybble
wordDigit1 w = fromIntegral (shift w (-12))
wordDigit2 :: Word -> Nybble
wordDigit2 w = fromIntegral (shift w (-8)) .&. nybMask
wordDigit3 :: Word -> Nybble
wordDigit3 w = fromIntegral (shift w (-4)) .&. nybMask
wordDigit4 :: Word -> Nybble
wordDigit4 w = fromIntegral w .&. nybMask

-- get the address (lowest 12 bits) from an opcode
opAddr :: Op -> Addr
opAddr op = op .&. addrMask

-- get a constant value (lowest 8 bits) from an opcode
opConst :: Op -> Byte
opConst op = fromIntegral op .&. byteMask
