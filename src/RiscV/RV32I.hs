{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module RiscV.RV32I
  ( Register(..)
  , CSRRegister(..)
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
  , CSRType(..)
  -- * Environment Call and Breakpoints
  , EnvironmentInstr(..)
  -- * Word Types
  , Word5(..)
  , Word12(..)
  , Word20(..)
  ) where

import Data.Bits
import Data.Word
import Test.QuickCheck (Arbitrary, arbitrary, oneof, elements)

data Instr
  = BranchInstr !BranchInstr
  | CSRInstr !CSRInstr
  | EnvironmentInstr !EnvironmentInstr
  | JumpInstr !JumpInstr
  | MemoryInstr !MemoryInstr
  | RRInstr !RegisterRegisterInstr
  | RIInstr !RegisterImmediateInstr
  | SyncInstr !SynchronizationInstr
  deriving (Show, Eq, Ord)

instance Arbitrary Instr where
  arbitrary =
    oneof
      [ BranchInstr <$> arbitrary
      , CSRInstr <$> arbitrary
      , EnvironmentInstr <$> arbitrary
      , JumpInstr <$> arbitrary
      , MemoryInstr <$> arbitrary
      , RRInstr <$> arbitrary
      , RIInstr <$> arbitrary
      , SyncInstr <$> arbitrary
      ]

data JumpInstr
  = JAL !Word20
        !Register
  | JALR !Word12
         !Register
         !Register
  deriving (Show, Eq, Ord)

instance Arbitrary JumpInstr where
  arbitrary =
    oneof
      [ JAL <$> arbitrary <*> arbitrary
      , JALR <$> arbitrary <*> arbitrary <*> arbitrary
      ]

data BranchCond
  = BEQ
  | BNE
  | BLT
  | BLTU
  | BGE
  | BGEU
  deriving (Show, Eq, Ord)

instance Arbitrary BranchCond where
  arbitrary = elements [BEQ, BNE, BLT, BLTU, BGE, BGEU]

data BranchInstr =
  Branch !Word12
         !BranchCond
         !Register
         !Register
  deriving (Show, Eq, Ord)

instance Arbitrary BranchInstr where
  arbitrary = Branch <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

data LoadWidth
  = Width !Width
  | HalfUnsigned
  | ByteUnsigned
  deriving (Show, Eq, Ord)

instance Arbitrary LoadWidth where
  arbitrary = oneof [Width <$> arbitrary, pure HalfUnsigned, pure ByteUnsigned]

data Width
  = Byte
  | Half
  | Word
  deriving (Show, Eq, Ord)

instance Arbitrary Width where
  arbitrary = elements [Byte, Half, Word]

data MemoryInstr
  = LOAD !LoadWidth
         !Word12
         !Register
         !Register
  | STORE !Width
          !Word12
          !Register
          !Register
  deriving (Show, Eq, Ord)

instance Arbitrary MemoryInstr where
  arbitrary =
    oneof
      [ LOAD <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      , STORE <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      ]

data IOpcode
  = ADDI
  | SLTI
  | SLTIU
  | XORI
  | ORI
  | ANDI
  deriving (Show, Eq, Ord)

instance Arbitrary IOpcode where
  arbitrary = elements [ADDI, SLTI, SLTIU, XORI, ORI, ANDI]

data ShiftOpcode
  = SLLI
  | SRLI
  | SRAI
  deriving (Show, Eq, Ord)

instance Arbitrary ShiftOpcode where
  arbitrary = elements [SLLI, SRLI, SRAI]

data RegisterImmediateInstr
  = IInstr !IOpcode
           !Word12
           !Register
           !Register
  | ShiftInstr !ShiftOpcode
               !Word5
               !Register
               !Register
  | LUI !Word20
        !Register
  | AUIPC !Word20
          !Register
  deriving (Show, Eq, Ord)

instance Arbitrary RegisterImmediateInstr where
  arbitrary =
    oneof
      [ IInstr <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      , ShiftInstr <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      , LUI <$> arbitrary <*> arbitrary
      , AUIPC <$> arbitrary <*> arbitrary
      ]

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
  deriving (Show, Eq, Ord)

instance Arbitrary ROpcode where
  arbitrary = elements [ADD, SLT, SLTU, AND, OR, XOR, SLL, SRL, SUB, SRA]

data RegisterRegisterInstr =
  RInstr !ROpcode
         !Register
         !Register
         !Register
  deriving (Show, Eq, Ord)

instance Arbitrary RegisterRegisterInstr where
  arbitrary = RInstr <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

data SyncOrdering = SyncOrd
  { deviceInput :: !Bool
  , deviceOutput :: !Bool
  , memoryReads :: !Bool
  , memoryWrites :: !Bool
  } deriving (Show, Eq, Ord)

instance Arbitrary SyncOrdering where
  arbitrary = SyncOrd <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

data SynchronizationInstr
  = FENCE !SyncOrdering
          !SyncOrdering
  | FENCEI
  deriving (Show, Eq, Ord)

instance Arbitrary SynchronizationInstr where
  arbitrary = oneof [FENCE <$> arbitrary <*> arbitrary, pure FENCEI]

data EnvironmentInstr
  = ECALL
  | EBREAK
  deriving (Show, Eq, Ord)

instance Arbitrary EnvironmentInstr where
  arbitrary = elements [ECALL, EBREAK]

-- | Control and status register instruction type
data CSRType
  = ReadWrite
  | ReadSet
  | ReadClear
  deriving (Show, Eq, Ord)

instance Arbitrary CSRType where
  arbitrary = elements [ReadWrite, ReadSet, ReadClear]

newtype CSRRegister = CSRRegister Word12 deriving (Show, Eq, Ord, Arbitrary)

-- | Control and Status Register Instructions
data CSRInstr
  = CSRRInstr !CSRType
              !CSRRegister
              !Register
              !Register
  | CSRIInstr !CSRType
              !CSRRegister
              !Word5
              !Register
  deriving (Show, Eq, Ord)

instance Arbitrary CSRInstr where
  arbitrary =
    oneof
      [ CSRRInstr <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      , CSRIInstr <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      ]

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
  deriving (Show, Eq, Ord)

instance Arbitrary Register where
  arbitrary =
    elements
      [ X0
      , X1
      , X2
      , X3
      , X4
      , X5
      , X6
      , X7
      , X8
      , X9
      , X10
      , X11
      , X12
      , X13
      , X14
      , X15
      , X16
      , X17
      , X18
      , X19
      , X20
      , X21
      , X22
      , X23
      , X24
      , X25
      , X26
      , X27
      , X28
      , X29
      , X30
      , X31
      ]

newtype Word5 = Word5 Word8 deriving (Show, Eq, Ord)

instance Arbitrary Word5 where
  arbitrary = do
    w8 <- arbitrary
    pure (Word5 $ w8 .&. 0x1F)

newtype Word12 = Word12 Word16 deriving (Show, Eq, Ord)

instance Arbitrary Word12 where
  arbitrary = do
    w16 <- arbitrary
    pure (Word12 $ w16 .&. 0x0FFF)

newtype Word20 = Word20 Word32 deriving (Show, Eq, Ord)

instance Arbitrary Word20 where
  arbitrary = do
    w32 <- arbitrary
    pure (Word20 $ w32 .&. 0x000FFFFF)
