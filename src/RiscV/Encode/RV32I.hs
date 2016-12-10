{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE BinaryLiterals #-}
module RiscV.Encode.RV32I
  (encodeInstr
  ) where

import Data.Bits
import Data.List (foldl')
import Data.Word
import Prelude hiding (reads)
import RiscV.RV32I

encodeInstr :: Instr -> Word32
encodeInstr (BranchInstr branchInst) = encodeBranchInst branchInst
encodeInstr (CSRInstr csrInstr) = encodeCSRInstr csrInstr
encodeInstr (EnvironmentInstr envInstr) = encodeEnvInstr envInstr
encodeInstr (JumpInstr jumpInstr) = encodeJumpInstr jumpInstr
encodeInstr (MemoryInstr memInstr) = encodeMemoryInstr memInstr
encodeInstr (RRInstr rrInstr) = encodeRegisterRegisterInstr rrInstr
encodeInstr (RIInstr riInstr) = encodeRegisterImmediateInstr riInstr
encodeInstr (SyncInstr syncInstr) = encodeSynchronizationInstr syncInstr

bitMaskFromTo :: Int -> Int -> Word32
bitMaskFromTo low high = foldl' (.|.) zeroBits (map bit [low..high])

-- | Bits not in the range are set to 0
bitsFromTo :: Int -> Int -> Word32 -> Word32
bitsFromTo low high w = w .&. bitMaskFromTo low high

-- | imm[12] | imm[10:5] | rs2 | rs1 | funct3 | imm[4:1] | imm[11] | opcode
encodeBranchInst :: BranchInstr -> Word32
encodeBranchInst (Branch (Word12 offset) cond src2 src1) =
  ((offset' .&. bit 11) `shiftL` 20) .|.
  ((bitsFromTo 4 9 offset') `shiftL` 21) .|.
  (encodeRegister src2 `shiftL` 20) .|.
  (encodeRegister src1 `shiftL` 15) .|.
  (encodeBranchCond cond `shiftL` 12) .|.
  ((bitsFromTo 0 3 offset') `shiftL` 8) .|.
  ((offset' .&. bit 10) `shiftR` 3) .|.
  0b1100011
  where
    offset' :: Word32
    offset' = fromIntegral offset

encodeBranchCond :: BranchCond -> Word32
encodeBranchCond BEQ = 0b000
encodeBranchCond BNE = 0b001
encodeBranchCond BLT = 0b100
encodeBranchCond BLTU = 0b101
encodeBranchCond BGE = 0b110
encodeBranchCond BGEU = 0b111

encodeCSRInstr :: CSRInstr -> Word32
encodeCSRInstr (CSRRInstr csrType (CSRRegister (Word12 csr)) src dest) =
  (fromIntegral csr `shiftL` 20) .|.
  (encodeRegister src `shiftL` 15) .|.
  (opcode' `shiftL` 12) .|.
  (encodeRegister dest `shiftL` 7) .|.
  0b1110011
  where
    opcode' =
      case csrType of
        ReadWrite -> 0b001
        ReadSet -> 0b010
        ReadClear -> 0b011
encodeCSRInstr (CSRIInstr csrType (CSRRegister (Word12 csr)) (Word5 immediate) dest) =
  (fromIntegral csr `shiftL` 20) .|.
  (fromIntegral immediate `shiftL` 15) .|.
  (opcode' `shiftL` 12) .|.
  (encodeRegister dest `shiftL` 7) .|.
  0b1110011
  where
    opcode' =
      case csrType of
        ReadWrite -> 0b101
        ReadSet -> 0b110
        ReadClear -> 0b111

encodeEnvInstr :: EnvironmentInstr -> Word32
encodeEnvInstr ECALL = 0b1110011
encodeEnvInstr EBREAK = setBit 0b1110011 20

encodeJumpInstr :: JumpInstr -> Word32
encodeJumpInstr (JAL (Word20 offset) dest) =
  ((offset' .&. bit 19) `shiftL` 12) .|.
  (bitsFromTo 0 9 offset' `shiftL` 21) .|.
  ((offset' .&. bit 10) `shiftL` 10) .|.
  (bitsFromTo 11 18 offset' `shiftR` 1) .|.
  (encodeRegister dest `shiftL` 7) .|.
  0b1101111
  where offset' = fromIntegral offset
encodeJumpInstr (JALR (Word12 offset) base dest) =
  (offset' `shiftL` 20) .|.
  (encodeRegister base `shiftL` 15) .|.
  -- 3 zeroes
  (encodeRegister dest `shiftL` 7) .|.
  0b1100111
  where offset' = fromIntegral offset

encodeWidth :: Width -> Word32
encodeWidth Byte = 0b000
encodeWidth Half = 0b001
encodeWidth Word = 0b010

encodeLoadWidth :: LoadWidth -> Word32
encodeLoadWidth (Width w) = encodeWidth w
encodeLoadWidth HalfUnsigned = 0b101
encodeLoadWidth ByteUnsigned = 0b100

encodeMemoryInstr :: MemoryInstr -> Word32
encodeMemoryInstr (LOAD width (Word12 offset) base dest) =
  (offset' `shiftL` 20) .|.
  (encodeRegister base `shiftL` 15) .|.
  (encodeLoadWidth width `shiftL` 12) .|.
  (encodeRegister dest `shiftL` 7) .|.
  0b0000011
  where offset' = fromIntegral offset
encodeMemoryInstr (STORE width (Word12 offset) src base) =
  (bitsFromTo 5 11 offset' `shiftL` 20) .|.
  (encodeRegister src `shiftL` 20) .|.
  (encodeRegister base `shiftL` 15) .|.
  (encodeWidth width `shiftL` 12) .|.
  (bitsFromTo 0 4 offset' `shiftL` 7) .|.
  0b0100011
  where offset' = fromIntegral offset

encodeROpcode :: ROpcode -> Word32
encodeROpcode oc =
  case oc of
    ADD -> 0b000
    SUB -> 0b000
    SLL -> 0b001
    SLT -> 0b010
    SLTU -> 0b011
    XOR -> 0b100
    SRL -> 0b101
    SRA -> 0b101
    OR -> 0b110
    AND -> 0b111

encodeRegisterRegisterInstr :: RegisterRegisterInstr -> Word32
encodeRegisterRegisterInstr (RInstr opcode src2 src1 dest) =
  (funct7 `shiftL` 25) .|.
  (encodeRegister src2 `shiftL` 20) .|.
  (encodeRegister src1 `shiftL` 15) .|.
  (encodeROpcode opcode `shiftL` 12) .|.
  (encodeRegister dest `shiftL` 7) .|.
  0b0110011
  where
    funct7 =
      case opcode of
        SUB -> 0b0100000
        SRA -> 0b0100000
        _ -> 0

encodeIOpcode :: IOpcode -> Word32
encodeIOpcode oc =
  case oc of
    ADDI -> 0b000
    SLTI -> 0b010
    SLTIU -> 0b011
    XORI -> 0b100
    ORI -> 0b110
    ANDI -> 0b111

encodeShiftOpcode :: ShiftOpcode -> Word32
encodeShiftOpcode oc =
  case oc of
    SLLI -> 0b001
    SRLI -> 0b101
    SRAI -> 0b101
encodeRegisterImmediateInstr :: RegisterImmediateInstr -> Word32
encodeRegisterImmediateInstr (IInstr opcode (Word12 immediate) src dest) =
  (immediate' `shiftL` 20) .|.
  (encodeRegister src `shiftL` 15) .|.
  (encodeIOpcode opcode `shiftL` 12) .|.
  (encodeRegister dest `shiftL` 7) .|.
  0b0010011
  where immediate' = fromIntegral immediate
encodeRegisterImmediateInstr (ShiftInstr opcode (Word5 shamt) src dest) =
  (funct7 `shiftL` 25) .|.
  (shamt' `shiftL` 20) .|.
  (encodeRegister src `shiftL` 15) .|.
  (encodeShiftOpcode opcode `shiftL` 12) .|.
  (encodeRegister dest `shiftL` 7) .|.
  0b0010011
  where shamt' = fromIntegral shamt
        funct7 = case opcode of
          SLLI -> 0
          SRLI -> 0
          SRAI -> 0b0100000
encodeRegisterImmediateInstr (LUI (Word20 immediate) dest) =
  (fromIntegral immediate `shiftL` 12) .|.
  (encodeRegister dest `shiftL` 7) .|.
  0b0110111
encodeRegisterImmediateInstr (AUIPC (Word20 immediate) dest) =
  (fromIntegral immediate `shiftL` 12) .|.
  (encodeRegister dest `shiftL` 7) .|.
  0b0010111

encodeSyncOrdering :: SyncOrdering -> Word32
encodeSyncOrdering (SyncOrd inp outp reads writes) =
  inp' .|. outp' .|. reads' .|. writes'
  where
    inp'
      | inp = 0b1000
      | otherwise = 0
    outp'
      | outp = 0b0100
      | otherwise = 0
    reads'
      | reads = 0b0010
      | otherwise = 0
    writes'
      | writes = 0b0001
      | otherwise = 0

encodeSynchronizationInstr :: SynchronizationInstr -> Word32
encodeSynchronizationInstr (FENCE pred succ) =
  (encodeSyncOrdering pred `shiftL` 24) .|.
  (encodeSyncOrdering succ `shiftL` 20) .|.
  0b0001111
encodeSynchronizationInstr FENCEI = setBit 0b0001111 12

encodeRegister :: Register -> Word32
encodeRegister X0 = 0
encodeRegister X1 = 1
encodeRegister X2 = 2
encodeRegister X3 = 3
encodeRegister X4 = 4
encodeRegister X5 = 5
encodeRegister X6 = 6
encodeRegister X7 = 7
encodeRegister X8 = 8
encodeRegister X9 = 9
encodeRegister X10 = 10
encodeRegister X11 = 11
encodeRegister X12 = 12
encodeRegister X13 = 13
encodeRegister X14 = 14
encodeRegister X15 = 15
encodeRegister X16 = 16
encodeRegister X17 = 17
encodeRegister X18 = 18
encodeRegister X19 = 19
encodeRegister X20 = 20
encodeRegister X21 = 21
encodeRegister X22 = 22
encodeRegister X23 = 23
encodeRegister X24 = 24
encodeRegister X25 = 25
encodeRegister X26 = 26
encodeRegister X27 = 27
encodeRegister X28 = 28
encodeRegister X29 = 29
encodeRegister X30 = 30
encodeRegister X31 = 31
