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
initRegs = array (0x0,0xf) [(i, 0x0) | i <- [0x0..0xf]]


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
initStack = []


data Machine = Machine { regs    :: Regs     -- | General-purpose registers
                       , mem     :: MemBlk   -- | System memory
                       , pc      :: Addr     -- | Program counter
                       , iReg    :: Addr     -- | I register
                       , sp      :: Maybe (Nybble) -- | Stack pointer
                       , stack   :: Stack    -- | Call stack
                       , dt      :: Byte     -- | Delay timer
                       , st      :: Byte     -- | Sound timer
                       , randGen :: StdGen   -- | Random number seed
                       , screen  :: Screen   -- | Screen
                       , kbd     :: Keyboard -- | Keyboard
                       , waiting :: Maybe (Nybble) -- | Which register (if any)
                                                   -- is waiting for a keystroke
                       , pxWrap  :: Bool     -- | Out-of-bounds pixel behavior
                       }

-- initial machine
initMachine :: StdGen -> Machine
initMachine gen = Machine initRegs initMem memStart 0x00 Nothing initStack 0 0
                          gen initScreen initKeyboard Nothing False

-- enter a program into a machine
loadMachine :: Addr -> Program -> Machine -> Machine
loadMachine a prog m = m { mem = mem' }
  where mem' = initMem // zip [a..memMax] prog

-- simulate one clock cycle of the machine
step :: Machine -> Result Machine
step m = if p >= memMax
         then Left "Reached end of memory."
         else if isJust (waiting m)
              then return m
              else applyOp op m
  where p  = pc m
        r  = mem m
        b1 = fromIntegral (r ! p) :: Word
        b2 = fromIntegral (r ! (p + 1)) :: Word
        op = shift b1 8 .|. b2

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
  Cls    -> incPC m { screen=initScreen }
  Ret    -> applyRet m
  Sys _  -> incPC m
  Jp   a -> Right $ applyJp a m
  Call a -> applyCall a m
  SeC  vx c  -> if cmpEC vx c m
                then incPC m >>= incPC
                else incPC m
  SneC vx c  -> if cmpNEC vx c m
                then incPC m >>= incPC
                else incPC m
  SeV  vx vy -> if cmpEV vx vy m
                then incPC m >>= incPC
                else incPC m
  LdC  vx c  -> incPC $ setReg vx c m
  AddC vx c  -> let x = getReg vx m
                    m' = setReg vx (x + c) m
                in incPC m'
  LdV  vx vy -> let y = getReg vy m
                in incPC $ setReg vx y m
  Or   vx vy -> let v = getReg vx m .|. getReg vy m
                in incPC $ setReg vx v m
  And  vx vy -> let v = getReg vx m .&. getReg vy m
                in incPC $ setReg vx v m
  Xor  vx vy -> let v = getReg vx m `xor` getReg vy m
                in incPC $ setReg vx v m
  AddV vx vy -> let x = getReg vx m
                    y = getReg vy m
                    m' = setReg vx (x + y) m
                in if x > byteMask - y
                   then incPC $ setVF m'
                   else incPC $ unsetVF m'
  Sub  vx vy -> let x = getReg vx m
                    y = getReg vy m
                    m' = setReg vx (x - y) m
                in if x > y
                   then incPC $ setVF m'
                   else incPC $ unsetVF m'
  Shr  vx    -> let x = getReg vx m
                    m' = setReg vx (shift x (-1)) m
                in if x .&. 0x01 == 0x01
                   then incPC $ setVF m'
                   else incPC $ unsetVF m'
  Subn vx vy -> let x = getReg vx m
                    y = getReg vy m
                    m' = setReg vx (y - x) m
                in if y > x
                   then incPC $ setVF m'
                   else incPC $ unsetVF m'
  Shl  vx    -> let x = getReg vx m
                    m' = setReg vx (shift x 1) m
                in if x .&. 0x80 == 0x80
                   then incPC $ setVF m'
                   else incPC $ unsetVF m'
  SneV vx vy -> if cmpNEV vx vy m
                then incPC m >>= incPC
                else incPC m
  LdI  a -> incPC m { iReg=a }
  JpV0 a -> let v = getReg 0x0 m
            in Right $ applyJp (fromIntegral v + a) m
  Rnd vx c -> let seed    = randGen m
                  (i, s') = next seed
                  r  = fromIntegral i .&. c
                  m' = setReg vx r m
              in incPC $ setRandGen s' m'
  Drw vx vy n -> let x = getReg vx m
                     y = getReg vy m
                     cs = readBytes (iReg m) n m
                     mscr = screen m
                     mpw = pxWrap m
                     (scr',col) = drawAt x y cs mpw mscr
                     m' = m { screen=scr' }
                 in if col
                    then incPC $ setVF m'
                    else incPC $ unsetVF m'
  Skp  vx -> let x = getReg vx m
             in if isDown x (kbd m)
                then incPC m >>= incPC
                else incPC m
  Sknp vx -> let x = getReg vx m
             in if isDown x (kbd m)
                then incPC m
                else incPC m >>= incPC
  LdDT vx -> incPC $ setReg vx (dt m) m
  LdK  vx -> incPC m { waiting=Just vx }
  SetDT vx -> let x = getReg vx m
              in incPC m { dt=x }
  SetST vx -> let x = getReg vx m
              in incPC m { st=x }
  AddI vx -> let x = getReg vx m
             in incPC $ m { iReg=iReg m + fromIntegral x }
  LdF  vx -> let x = getReg vx m
             in incPC $ m { iReg=charPos x }
  WrtB vx -> let x = getReg vx m
                 cs = bcd x
             in incPC $ writeBytes (iReg m) cs m
  WrtV vx -> let cs = take (1 + fromIntegral vx) (elems (regs m))
             in incPC $ writeBytes (iReg m) cs m
  RdV  vx -> let cs = readBytes (iReg m) (vx + 1) m
                 regs' = regs m // zip [0x0..vx] cs
             in incPC m { regs=regs' }


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

-- increment the program counter once (it goes up by two bytes)
incPC :: Machine -> Result Machine
incPC m = if pc' > memMax
          then Left "Reached end of memory."
          else Right m { pc=pc'}
  where pc' = pc m + 2

-- set the new random generator
setRandGen :: StdGen -> Machine -> Machine
setRandGen r m = m { randGen = r }

-- set the overflow/underflow bit
setVF :: Machine -> Machine
setVF m = setReg 0xf 0x01 m
unsetVF :: Machine -> Machine
unsetVF m = setReg 0xf 0x00 m

-- All these compare two values and give a value of type Bool
cmpEC  vx c  m = getReg vx m == c
cmpNEC vx c  m = not $ cmpEC vx c m
cmpEV  vx vy m = getReg vx m == getReg vy m
cmpNEV vx vy m = not $ cmpEV vx vy m

-- Read a byte from the machine's memory
readByte :: Addr -> Machine -> Byte
readByte a m = if validAddr a
               then ram ! a
               -- this should never happen
               else ram ! (a `mod` memMax)
  where ram = mem m

-- read n bytes
readBytes :: Integral a => Addr -> a -> Machine -> [Byte]
readBytes _ 0 _ = []
readBytes a n m = readByte a m : readBytes (a + 1) (n - 1) m

-- write list of bytes to the machine's memory
writeBytes :: Addr -> [Byte] -> Machine -> Machine
writeBytes a cs m = m { mem=mem' }
  where len   = fromIntegral (length cs)
        end   = a + len - 1
        range = if end > memMax
                then [a..memMax]
                else [a..end]
        mem'  = mem m // zip range cs

-- Set pc to a
applyJp :: Addr -> Machine -> Machine
applyJp a m = m { pc = a }

-- Call a subroutine
applyCall :: Addr -> Machine -> Result Machine
applyCall a m = case msp of
  Nothing    -> Right $ applyJp a m { sp=Just 1, stack=stack' }
  Just i     -> if i >= maxSP
                then Left $ "Error encountered at " ++ show msp ++
                     ": Call stack exhausted."
                else Right $ applyJp a m { sp=Just (i + 1), stack=stack' }
  where msp = sp m
        stack' = pc m : stack m

-- Return from a subroutine
applyRet :: Machine -> Result Machine
applyRet m = case msp of
  Nothing -> Left $ "Error encountered at " ++ show msp ++
             ": Cannot return from empty call stack."
  Just i  -> let m' = m { pc=pc', sp=Just (i - 1), stack=tail mstack }
             in incPC m'
  where msp = sp m
        mstack = stack m
        pc' = head mstack
