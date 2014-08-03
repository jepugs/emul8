module Emul8.Instr where

import Emul8.Core

-- | Instructions are given in ascending order of the minimum opcode value
data Instr = Cls
           | Ret
           | Sys  Addr
           | Jp   Addr
           | Call Addr
           | SeC  Vx Byte
           | SneC Vx Byte
           | SeV  Vx Vy
           | LdC  Vx Byte
           | AddC Vx Byte
           | LdV  Vx Vy
           | Or   Vx Vy
           | And  Vx Vy
           | Xor  Vx Vy
           | AddV Vx Vy
           | Sub  Vx Vy
           | Shr  Vx
           | Subn Vx Vy
           | Shl  Vx
           | SneV Vx Vy
           | LdI  Addr
           | JpV0 Addr
           | Rnd  Vx Byte
           | Drw  Vx Vy Nybble
           | Skp  Vx
           | Sknp Vx
           | LdDT Vx
           | LdK  Vx
           | SetDT Vx
           | SetST Vx
           | AddI Vx
           | LdF  Vx
           | WrtB  Vx
           | WrtV Vx
           | RdV  Vx

-- | Generate an instruction from a given opcode
parseOp :: Op -> Either String Instr
parseOp op = case d1 of
  0x0 -> case (d2,d3,d4) of
      (0x0,0xe,0x0) -> Right Cls
      (0x0,0xe,0xe) -> Right Ret
      _ -> Right (Sys addr)
  0x1 -> Right (Jp addr)
  0x2 -> Right (Call addr)
  0x3 -> Right (SeC vx c)
  0x4 -> Right (SneC vx c)
  0x5 -> Right (SeV vx vy)
  0x6 -> Right (LdC vx c)
  0x7 -> Right (AddC vx c)
  0x8 -> case d4 of
    0x0 -> Right (LdV vx vy)
    0x1 -> Right (Or vx vy)
    0x2 -> Right (And vx vy)
    0x3 -> Right (Xor vx vy)
    0x4 -> Right (AddV vx vy)
    0x5 -> Right (Sub vx vy)
    0x6 -> Right (Shr vx)
    0x7 -> Right (Subn vx vy)
    0xe -> Right (Shl vx)
  0x9 -> case d4 of
    0x0 -> Right (SneV vx vy)
    _   -> wrong
  0xa -> Right (LdI addr)
  0xb -> Right (JpV0 addr)
  0xc -> Right (Rnd vx c)
  0xd -> Right (Drw vx vy n)
  0xe -> case (d3,d4) of
    (0x9,0xe) -> Right (Skp vx)
    (0xa,0x1) -> Right (Sknp vx)
    _         -> wrong
  0xf -> case (d3,d4) of
    (0x0,0x7) -> Right (LdDT vx)
    (0x0,0xa) -> Right (LdK vx)
    (0x1,0x5) -> Right (SetDT vx)
    (0x1,0x8) -> Right (SetST vx)
    (0x1,0xe) -> Right (AddI vx)
    (0x2,0x9) -> Right (LdF vx)
    (0x3,0x3) -> Right (WrtB vx)
    (0x5,0x5) -> Right (WrtV vx)
    (0x6,0x5) -> Right (RdV vx)
    _         -> wrong
  where d1 = wordDigit1 op
        d2 = wordDigit2 op
        d3 = wordDigit3 op
        d4 = wordDigit4 op
        addr = opAddr op
        vx = d2
        vy = d3
        c = opConst op
        n = d4
        wrong = Left "Invalid opcode."
