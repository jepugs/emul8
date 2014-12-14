module Dis8.Disassemble where

import Common.Core
import Common.Instr

import Control.Applicative
import Data.Bits
import Data.List
import Data.Maybe


-- | Pretty-print a list of instructions starting at 0x200
showProg :: Program -> String
showProg p = showProg' 0x200 (genLabels $ parseInstrs p) p
  where maxAddr = 0x199 + fromIntegral (length p)
        lbls = pruneLabels 0x200 maxAddr $ genLabels $ parseInstrs p

-- | An address label
type Label = (Addr,String)


-- | Pretty-print a list of instructions given an address, labels, and
-- indentation.
showProg' :: Addr -> [Label] -> Program -> String
showProg' a _    []         = ""
showProg' a lbls (b:[])     = showByte b ++ "\n"
showProg' a lbls (b1:b2:bs) = lstr ++ istr ++ showProg' a' lbls bs
  where a' = a + 2
        op = makeOp b1 b2
        instr = parseOp op
        istr = either (const bytes) showi instr  -- If there is an invalid
                                                 -- opcode, then
        bytes = "  " ++ showByte b1 ++ "\n  " ++ showByte b2 ++ "\n"
        lstr = case lookup a lbls of  -- If the current address is labeled, then
                                      -- give the label's name.
          Just n  -> n ++ ":\n"
          Nothing -> ""
        showi i = "  " ++ showInstr lbls i ++ "\n"

-- | Tell if an instruction is a skip instruction.
isSkip :: Instr -> Bool
isSkip (SeC  _ _) = True
isSkip (SneC _ _) = True
isSkip (SeV  _ _) = True
isSkip (SneV _ _) = True
isSkip (Skp  _  ) = True
isSkip (Sknp _  ) = True
isSkip _          = False

-- | Remove labels with addresses outside of the given range.
pruneLabels :: Addr -> Addr -> [Label] -> [Label]
pruneLabels min max ls = filter ff ls
  where ff (a,_) = a >= min && a <= max

-- | Generate labels from a list of instructions.
genLabels :: [Instr] -> [Label]
genLabels is = ls
  where as = nub $ map fromJust $ filter isJust $ map instrAddr is
        ls = zip (sort as) ["L" ++ show x | x <- [0..]]
        mkl i a = (a,"L" ++ show i)

-- | Get an instruction's address if it has one.
instrAddr :: Instr -> Maybe Addr
instrAddr (Sys  a) = Just a
instrAddr (Jp   a) = Just a
instrAddr (Call a) = Just a
instrAddr (LdI  a) = Just a
instrAddr (JpV0 a) = Just a
instrAddr _        = Nothing

-- | Convert a list of bytes to instructions, ignoring invalid opcodes.
parseInstrs :: Program -> [Instr]
parseInstrs []        = []
parseInstrs (p:[])    = []
parseInstrs (p:p':ps) = either (const next) (:next) $ parseOp op
  where op = makeOp p p'
        next = parseInstrs ps

-- | Pretty-print an instruction.
showInstr :: [Label] -> Instr -> String
showInstr lbls i = case i of
  Cls      -> "clr"
  Ret      -> "ret"
  Sys  a   -> case lookup a lbls of
    Just l  -> "sys  " ++ l
    Nothing -> "sys  " ++ show a
  Jp   a   -> case lookup a lbls of
    Just l  -> "jump " ++ l
    Nothing -> "jump " ++ show a
  Call a   -> case lookup a lbls of
    Just l  -> "call " ++ l
    Nothing -> "call " ++ show a
  SeC  x c -> "skipeq   " ++ showReg x ++ " " ++ show c
  SneC x c -> "skipneq  " ++ showReg x ++ " " ++ show c
  SeV  x y -> "skipeq   " ++ showReg x ++ " " ++ showReg y
  LdC  x c -> "load " ++ showReg x ++ " " ++ show c
  AddC x c -> "add  " ++ showReg x ++ " " ++ show c
  LdV  x y -> "load " ++ showReg x ++ " " ++ showReg y
  Or   x y -> "or   " ++ showReg x ++ " " ++ showReg y
  And  x y -> "and  " ++ showReg x ++ " " ++ showReg y
  Xor  x y -> "xor  " ++ showReg x ++ " " ++ showReg y
  AddV x y -> "add  " ++ showReg x ++ " " ++ showReg y
  Sub  x y -> "sub  " ++ showReg x ++ " " ++ showReg y
  Shr  x   -> "shr  " ++ showReg x
  Subn x y -> "subn " ++ showReg x ++ " " ++ showReg y
  Shl  x   -> "shl  " ++ showReg x
  SneV x y -> "skipneq " ++ showReg x ++ " " ++ showReg y
  LdI  a   -> case lookup a lbls of
    Just l  -> "load I  " ++ l
    Nothing -> "load I  " ++ show a
  JpV0 a   -> case lookup a lbls of
    Just l  -> "jpv0 " ++ l
    Nothing -> "jpv0 " ++ show a
  Rnd  x c -> "rnd  " ++ showReg x ++ " " ++ show c
  Drw  x y h->"drw  " ++ showReg x ++ " " ++ showReg y ++ " " ++ show h
  Skp  x   -> "skipkey  " ++ showReg x
  Sknp x   -> "skipnkey " ++ showReg x
  LdDT x   -> "load " ++ showReg x ++ " " ++ "DT"
  LdK  x   -> "key  " ++ showReg x
  SetDT x  -> "load DT " ++ showReg x
  SetST x  -> "load ST " ++ showReg x
  AddI x   -> "add  I  " ++ showReg x
  LdF  x   -> "char " ++ showReg x
  WrtB x   -> "bcd  " ++ showReg x
  WrtV x   -> "push " ++ showReg x
  RdV  x   -> "read " ++ showReg x

-- | Print out a byte pseudo-instruction.
showByte :: Byte -> String
showByte = ("byte " ++) . show

-- | Print out a register name.
showReg :: Vx -> String
showReg x = 'V' : c : ""
  where c = case x of
          0x0 -> '0'
          0x1 -> '1'
          0x2 -> '2'
          0x3 -> '3'
          0x4 -> '4'
          0x5 -> '5'
          0x6 -> '6'
          0x7 -> '7'
          0x8 -> '8'
          0x9 -> '9'
          0xa -> 'A'
          0xb -> 'B'
          0xc -> 'C'
          0xd -> 'D'
          0xe -> 'E'
          0xf -> 'F'
