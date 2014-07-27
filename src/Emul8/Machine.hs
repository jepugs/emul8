module Emul8.Machine where

import Emul8.Core
import Emul8.Font
import Emul8.Instr
import Emul8.Keyboard
import Emul8.Screen

import Control.Applicative
import Data.Array
import Data.Bits
import Data.Maybe (isJust, fromJust)
import System.Random


-- Registers
type Regs = Array Nybble Byte

-- initial registers (all zeroes)
initRegs :: Regs
initRegs = array (0x0,0xf) [(i, 0x0) | i <- [0..0xf]]


-- Memory block
type MemBlk = Array Addr Byte

-- where the font starts
fontStart = 0x000 :: Addr
fontEnd = fontStart + fontLen

-- get the position of a character sprite
charPos :: Nybble -> Addr
charPos n = fontStart + charPosns !! fromIntegral (ensureNyb n)

-- where the program counter starts out
memStart = 0x200 :: Addr

-- empty memory block (all zeroes)
emptyMem :: MemBlk
emptyMem = array (memMin,memMax) [(i, 0x00) | i <- [memMin..memMax]]

-- initial memory block (with the font loaded starting at fontStart)
initMem :: MemBlk
initMem = emptyMem // [(i,font !! (fromIntegral i)) | i <- [fontStart..fontEnd]]


-- maximum stack pointer value
maxSP = 16 :: Nybble

-- stack
type Stack = [Addr]

-- initial stack (empty)
initStack :: Stack
initStack = replicate (fromIntegral maxSP) 0x000

data Machine = Machine { regs    :: Regs     -- general-purpose registers
                       , mem     :: MemBlk   -- system memory
                       , pc      :: Addr     -- program counter
                       , iReg    :: Addr     -- I register
                       , sp      :: Maybe (Nybble) -- stack pointer
                       , stack   :: Stack    -- call stack
                       , dt      :: Byte     -- delay timer
                       , st      :: Byte     -- sound timer
                       , randGen :: StdGen   -- random number seed
                       , screen  :: Screen   -- screen
                       , kbd     :: Keyboard -- keyboard
                       , waiting :: Maybe (Nybble) -- which register (if any) is
                                                   -- waiting for a keystroke
                       }

-- initial machine
initMachine :: StdGen -> Machine
initMachine gen = Machine initRegs initMem memStart 0x00 Nothing initStack 0 0
                          gen initScreen initKeyboard Nothing

-- enter a program into a machine
load :: Program -> StdGen -> Machine
load prog gen = (initMachine gen) { mem = mem' }
  where mem' = initMem // zip [memStart..memMax] prog

-- simulate one clock cycle of the machine
step :: Machine -> Result Machine
step m = applyOp op m >>= incPC
  where p  = pc m
        r  = mem m
        b1 = fromIntegral (r ! p) :: Word
        b2 = fromIntegral (r ! p + 1) :: Word
        op = shift b1 8 .&. b2

-- update all of the machine's timers one step
updateTimers :: Machine -> Machine
updateTimers m = m { dt=dt', st=st' }
  where mdt = dt m
        dt' = if mdt == 0
              then 0
              else mdt - 1
        mst = st m
        st' = if mst == 0
              then 0
              else mst - 1

applyOp :: Op -> Machine -> Result Machine
applyOp o m = parseOp o >>= flip applyInstr m

applyInstr :: Instr -> Machine -> Result Machine
applyInstr i m = case i of
  Cls    -> Right m
  Ret    -> applyRet m
  Sys _  -> Right m
  Jp   a -> Right $ applyJp a m
  Call a -> applyCall a m
  SeC  vx c  -> if cmpEC vx c m
                then incPC m
                else return m
  SneC vx c  -> if cmpNEC vx c m
                then incPC m
                else return m
  SeV  vx vy -> if cmpEV vx vy m
                then incPC m
                else Right m
  LdC  vx c  -> Right $ setReg vx c m
  AddC vx c  -> let x = getReg vx m
                    m' = setReg vx (x + c) m
                in if x > byteMask - c
                   then Right $ setVF m'
                   else Right $ unsetVF m'
  LdV  vx vy -> let y = getReg vy m
                in Right $ setReg vx y m
  Or   vx vy -> let v = getReg vx m .|. getReg vx m
                in Right $ setReg vx v m
  And  vx vy -> let v = getReg vx m .&. getReg vy m
                in Right $ setReg vx v m
  Xor  vx vy -> let v = xor (getReg vx m) (getReg vy m)
                in Right $ setReg vx v m
  AddV vx vy -> let x = getReg vx m
                    y = getReg vy m
                    m' = setReg vx (x + y) m
                in if x > byteMask - y
                   then Right $ setVF m'
                   else Right $ unsetVF m'
  Sub  vx vy -> let x = getReg vx m
                    y = getReg vy m
                    m' = setReg vx (x - y) m
                in if x > y
                   then Right $ setVF m'
                   else Right $ unsetVF m'
  Shr  vx    -> let x = getReg vx m
                    m' = setReg vx (shift x (-1)) m
                in if x .&. 0x01 == 0x01
                   then Right $ setVF m'
                   else Right $ unsetVF m'
  Subn vx vy -> let x = getReg vx m
                    y = getReg vy m
                    m' = setReg vx (y - x) m
                in if y > x
                   then Right $ setVF m'
                   else Right $ unsetVF m'
  Shl  vx    -> let x = getReg vx m
                    m' = setReg vx (shift x 1) m
                in if x .&. 0x08 == 0x08
                   then Right $ setVF m'
                   else Right $ unsetVF m'
  SneV vx vy -> if cmpNEV vx vy m
                then incPC m
                else return m
  LdI  a -> return m { iReg=a }
  JpV0 a -> let v = getReg 0x0 m
            in Right $ applyJp (fromIntegral v + a) m
  Rnd vx c -> let seed   = randGen m
                  (i, n) = next seed
                  m' = setReg vx (fromIntegral i .&. c) m
              in Right $ setRandGen n m
  Drw vx vy w -> let x = getReg vx m
                     y = getReg vy m
                     cs = readBytes (iReg m) w m
                     scr = screen m
                 in return (m { screen=drawAt x y cs scr })
  Skp  vx -> if isDown (ensureNyb vx) (kbd m)
             then incPC m
             else Right m
  Sknp vx -> if isUp   (ensureNyb vx) (kbd m)
             then incPC m
             else Right m
  LdDT vx -> Right $ setReg vx (dt m) m
  LdK  vx -> Right m { waiting=Just vx }
  SetDT vx -> let x = getReg vx m
              in Right m { dt=x }
  SetST vx -> let x = getReg vx m
              in return m { st=x }
  AddI vx -> let x = getReg vx m
             in Right $ m { iReg=iReg m + fromIntegral x }
  LdF  vx -> let x = getReg vx m
             in Right $ m { iReg=charPos x }
  WrtB vx -> let x = getReg vx m
                 cs = bcd x
             in Right $ writeBytes (iReg m) cs m
  WrtV vx -> let cs = take (fromIntegral vx) (elems (regs m))
             in Right $ writeBytes (iReg m) cs m
  RdV  vx -> let cs = readBytes (iReg m) vx m
                 regs' = regs m // zip [0x0..vx] cs
             in Right m { regs=regs' }


-- convert a value to its BCD representation
bcd :: Byte -> [Byte]
bcd c = [d1,d2,d3]
  where d1 = c `div` 100
        d2 = c `div` 10 `mod` 10
        d3 = c `mod` 10

-- get a value stored in a register
getReg :: Vx -> Machine -> Byte
getReg x m = (regs m ! ensureNyb x)

-- set the value of a register
setReg :: Vx -> Byte -> Machine -> Machine
setReg x c m = m { regs=regs' }
  where regs' = regs m // [(ensureNyb x,c)]

-- increment the program counter once
incPC :: Machine -> Result Machine
incPC m = if pc' > memMax
          then Left "Reached end of memory."
          else Right (m { pc=pc'})
  where pc' = pc m + 1

-- set the new random generator
setRandGen :: StdGen -> Machine -> Machine
setRandGen r m = m { randGen = r }

-- set the overflow/underflow bit
setVF :: Machine -> Machine
setVF m = setReg 0xf vf' m
  where vf' = getReg 0xf m .|. 0x01
unsetVF :: Machine -> Machine
unsetVF m = setReg 0xf vf' m
  where vf' = getReg 0xf m .&. 0xfe

-- All these compare two values and give a value of type Bool
cmpEC  vx c  m = c == getReg vx m
cmpNEC vx c  m = not (cmpEC vx c m)
cmpEV  vx vy m = getReg vx m == getReg vy m
cmpNEV vx vy m = not (cmpEV vx vy m)

-- Read a byte from the machine's memory
readByte :: Addr -> Machine -> Byte
readByte a m = if validAddr a
               then ram ! a
               -- this should never happen
               else ram ! (a `mod` memMax)
  where ram = mem m

-- read n bytes
readBytes :: Integral a => Addr -> a -> Machine -> [Byte]
readBytes 0 _ _ = []
readBytes a n m = readByte a m : readBytes (a + 1) (n - 1) m

-- write list of bytes to the machine's memory
writeBytes :: Addr -> [Byte] -> Machine -> Machine
writeBytes a cs m = m { mem=mem' }
  where len   = fromIntegral (length cs)
        end   = a + len
        range = if end > memMax
                then [a..memMax]
                else [a..len - 1]
        mem'  = mem m // zip range cs

-- Set pc to a
applyJp :: Addr -> Machine -> Machine
applyJp a m = m { pc = a }

-- Call a subroutine
applyCall :: Addr -> Machine -> Result Machine
applyCall a m = case msp of
  Nothing -> Left "Call stack exhausted."
  Just i  -> Right $ applyJp a m { sp=Just (i + 1), stack=stack' }
  where msp = sp m
        stack' = a : stack m

-- Return from a subroutine
applyRet :: Machine -> Result Machine
applyRet m = case msp of
  Nothing -> Left "Can not return with empty call stack."
  Just i  -> Right $ m { sp=Just (i - 1), stack=tail mstack }
  where msp = sp m
        mstack = stack m
