{-# LANGUAGE PatternSynonyms #-}
module RiscV.RV32I
  ( Register(..)
  , Instr(..)
  -- * Integer Register-Immediate Instructions
  , RegisterImmediateInstr(..)
  , IOpcode(..)
  , ShiftOpcode(..)
  -- * Integer Register-Register Instructions
  , RegisterRegisterInstr(..)
  , ROpcode(..)
  -- * Control Transfer Instructions
  , JumpInstr(..)
  , BranchInstr(..)
  , BranchCond(..)
  -- * Load and Store Instructions
  , MemoryInstr(..)
  , Width(..)
  , LoadWidth(..)
  -- * Memory Synchronization Instructions
  , SynchronizationInstr(..)
  , SyncOrdering(..)
  -- * Control and Status Register Instructions
  , CSRInstr(..)
  , CSRIOpcode(..)
  , CSRROpcode(..)
  -- * Environment Call and Breakpoints
  , EnvironmentInstr(..)
  -- * Word Types
  , Word5(..)
  , Word12(..)
  , Word20(..)
  ) where

import Data.Word

data Instr
  = BranchInstr !BranchInstr
  | CSRInstr !CSRInstr
  | EnvironmentInstr !EnvironmentInstr
  | JumpInstr !JumpInstr
  | MemoryInstr !MemoryInstr
  | RRInstr !RegisterImmediateInstr
  | RIInstr !RegisterRegisterInstr
  | SyncInstr !SynchronizationInstr

data JumpInstr
  = JAL !Word20
        !Register
  | JALR !Word12
         !Register

data BranchCond
  = BEQ
  | BNE
  | BLT
  | BLTU
  | BGE
  | BEGU

data BranchInstr =
  Branch !Word20
         !BranchCond
         !Register
         !Register

data LoadWidth
  = Width !Width
  | HalfUnsigned
  | ByteUnsigned

data Width
  = Byte
  | Half
  | Word

data MemoryInstr
  = LOAD !LoadWidth
         !Word12
         !Register
         !Register
  | STORE !Width
          !Word12
          !Register
          !Register

data IOpcode
  = ADDI
  | SLTI
  | SLTIU
  | XORI
  | ORI
  | ANDI

data ShiftOpcode
  = SLLI
  | SRLI
  | SRAI

data RegisterImmediateInstr
  = IInstr !IOpcode
           !Word20
           !Register
           !Register
  | ShiftInstr !Word5
               !Register
               !Register
  | LUI !Word20 !Register
  | AUIPC !Word20 !Register

data ROpcode
  = ADD
  | SLT
  | SLTU
  | AND
  | OR
  | XOR
  | SLL
  | SRL
  | SUB
  | SRA

data RegisterRegisterInstr =
  RInstr !ROpcode
         !Register
         !Register
         !Register

data SyncOrdering = SyncOrd
  { deviceInput :: !Bool
  , deviceOutput :: !Bool
  , memoryReads :: !Bool
  , memoryWrites :: !Bool
  }

data SynchronizationInstr
  = FENCE !SyncOrdering
          !SyncOrdering
  | FENCEI

data EnvironmentInstr
  = ECALL
  | EBREAK

-- | Control and status register instruction opcode using a register
data CSRROpcode
  = CSRRW
  | CSRRS
  | CSRRC

-- | Control and status register instruction opcode using an immediate
data CSRIOpcode
  = CSRRWI
  | CSRRSI
  | CSRCI

-- | Control and Status Register Instructions
data CSRInstr
  = CSRRInstr !CSRROpcode
              !Register
              !Register
  | CSRIInstr !CSRROpcode
              !Word5
              !Register

-- | Register 1-31 are general-purpose registers holding integer
-- values.
--
-- Register 0 is hardwired to the constant 0.
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

newtype Word5 = Word5 Word8
newtype Word12 = Word12 Word16
newtype Word20 = Word20 Word32
