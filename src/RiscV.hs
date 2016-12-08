{-# LANGUAGE PatternSynonyms #-}
module RiscV
  ( Register(..)
  , InstrI(..)
  ) where


import Data.Word

-- | Register 1-31 are general-purpose registers holding integer
-- values. Register 0 is hardwired to the constant 0. PC is the
-- program counter.
data Register
  = X0
  | X1
  | X2
  | X3
  | X4
  | X5
  | X6
  | X7
  | X8
  | X9
  | X10
  | X11
  | X12
  | X13
  | X14
  | X15
  | X16
  | X17
  | X18
  | X19
  | X20
  | X21
  | X22
  | X23
  | X24
  | X25
  | X26
  | X27
  | X28
  | X29
  | X30
  | X31
  | PC

newtype Word5 = Word5 Word8
newtype Word12 = Word12 Word16
newtype Word20 = Word20 Word32

-- | Integer Register-Immediate Instructions
data InstrI
  = ADDI !Word12
         !Register
         !Register
  | SLTI !Word12
         !Register
         !Register
  | SLTIU !Word12
          !Register
          !Register
  | ANDI !Word12
         !Register
         !Register
  | ORI !Word12
        !Register
        !Register
  | XORI !Word12
         !Register
         !Register
  | SLLI !Word5
         !Register
         !Register
  | SRLI !Word5
         !Register
         !Register
  | SRAI !Word5
         !Register
         !Register
  | LUI !Word20
        !Register
  | AUIPC !Word20
          !Register

-- | Integer Register-Register Operations
data InstrR
  = ADD !Register
        !Register
        !Register
  | SLT !Register
        !Register
        !Register
  | SLTU !Register
         !Register
         !Register
  | AND !Register
        !Register
        !Register
  | OR !Register
       !Register
       !Register
  | XOR !Register
        !Register
        !Register
  | SLL !Register
        !Register
        !Register
  | SRL !Register
        !Register
        !Register
  | SUB !Register
        !Register
        !Register
  | SRA !Register
        !Register
        !Register
