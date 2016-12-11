{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE FlexibleContexts #-}
module RiscV.Decode.RV32I
  ( decodeInstr
  , DecodingError(..)
  ) where

import Control.Monad.Except
import Data.Bits
import Data.Monoid
import Data.Word
import RiscV.Internal.Util
import RiscV.RV32I

data DecodingError = DecodingError { errorMsg :: String }

decodeInstr :: MonadError DecodingError m => Word32 -> m Instr
decodeInstr word =
  case opCode of
    0b1100011 -> BranchInstr <$> decodeBranchInstr word
    0b1110011 -> decodeCSROrEnvInstr word
    0b1101111 -> JumpInstr <$> decodeJALInstr word
    0b1100111 -> JumpInstr <$> decodeJALRInstr word
    0b0000011 -> MemoryInstr <$> decodeLoadInstr word
    0b0100011 -> MemoryInstr <$> decodeStoreInstr word
    0b0110011 -> RRInstr <$> decodeRRInstr word
    0b0010011 -> RIInstr <$> decodeRIInstr word
    0b0110111 -> RIInstr <$> decodeLUIInstr word
    0b0010111 -> RIInstr <$> decodeAUIPCInstr word
    0b0001111 -> SyncInstr <$> decodeSyncInstr word
    _ -> throwError $ DecodingError ("Unsupported opcode: " <> show opCode)
  where
    opCode = bitsFromTo 0 7 word

decodeBranchCond :: MonadError DecodingError m => Word32 -> m BranchCond
decodeBranchCond cond =
  case cond of
    0b000 -> pure BEQ
    0b001 -> pure BNE
    0b100 -> pure BLT
    0b101 -> pure BGE
    0b110 -> pure BLTU
    0b111 -> pure BGEU
    _ ->
      throwError $ DecodingError ("Unsupported branch condition: " <> show cond)

decodeBranchInstr :: MonadError DecodingError m => Word32 -> m BranchInstr
decodeBranchInstr word = do
  cond <- decodeBranchCond (extractBits 12 14 word)
  pure $ Branch (Word12 $ fromIntegral imm) cond (decodeRegister src2) (decodeRegister src1)
  where
    imm12 = extractBits 31 31 word
    imm10to5 = extractBits 25 30 word
    imm4to1 = extractBits 8 11 word
    imm11 = extractBits 7 7 word
    imm =
      (imm12 `shiftL` 11) .|. (imm11 `shiftL` 10) .|. (imm10to5 `shiftL` 4) .|.
      imm4to1
    src2 = extractBits 20 24 word
    src1 = extractBits 15 19 word

decodeCSROrEnvInstr :: MonadError DecodingError m => Word32 -> m Instr
decodeCSROrEnvInstr word =
  case opCode of
    0b000 -> EnvironmentInstr <$> decodeEnvInstr word
    _ -> CSRInstr <$> decodeCSRInstr word
  where opCode = extractBits 12 14 word

decodeEnvInstr :: Applicative m => Word32 -> m EnvironmentInstr
decodeEnvInstr word =
  if testBit word 20
    then pure ECALL
    else pure EBREAK

decodeCSRType :: MonadError DecodingError m => Word32 -> m CSRType
decodeCSRType twoBits = case twoBits of
  0b01 -> pure ReadWrite
  0b10 -> pure ReadSet
  0b11 -> pure ReadClear
  _ -> throwError $ DecodingError ("Unsupported csr type: " <> show twoBits)

decodeCSRInstr :: MonadError DecodingError m => Word32 -> m CSRInstr
decodeCSRInstr word = do
  csrType <- decodeCSRType (extractBits 12 13 word)
  if testBit word 14
    then let src = decodeRegister (extractBits 15 19 word)
         in pure $ CSRRInstr csrType csr src dest
    else let zimm = Word5 . fromIntegral $ extractBits 15 19 word
         in pure $ CSRIInstr csrType csr zimm dest
  where
    csr = CSRRegister . Word12 . fromIntegral $ extractBits 20 31 word
    dest = decodeRegister (extractBits 7 11 word)

decodeJALInstr :: MonadError DecodingError m => Word32 -> m JumpInstr
decodeJALInstr word = pure (JAL (Word20 imm) dest)
  where
    dest = decodeRegister (extractBits 7 11 word)
    imm20 = extractBits 31 31 word
    imm10to1 = extractBits 21 30 word
    imm11 = extractBits 20 20 word
    imm19to12 = extractBits 12 19 word
    imm =
      (imm20 `shiftL` 19) .|. (imm19to12 `shiftL` 11) .|. (imm11 `shiftL` 10) .|.
      imm10to1

decodeJALRInstr :: MonadError DecodingError m => Word32 -> m JumpInstr
decodeJALRInstr word = pure (JALR (Word12 offset) base dest)
  where
    dest = decodeRegister (extractBits 7 11 word)
    base = decodeRegister (extractBits 15 19 word)
    offset = fromIntegral (extractBits 20 31 word)

decodeLoadWidth :: MonadError DecodingError m => Word32 -> m LoadWidth
decodeLoadWidth threeBits =
  case threeBits of
    0b000 -> pure (Width Byte)
    0b001 -> pure (Width Half)
    0b010 -> pure (Width Word)
    0b100 -> pure ByteUnsigned
    0b101 -> pure HalfUnsigned
    _ -> throwError $ DecodingError ("Unsupported load width: " <> show threeBits)

decodeLoadInstr :: MonadError DecodingError m => Word32 -> m MemoryInstr
decodeLoadInstr word = do
  width <- decodeLoadWidth (extractBits 12 14 word)
  pure (LOAD width offset base dest)
  where offset = Word12 (fromIntegral (extractBits 20 31 word))
        base = decodeRegister (extractBits 15 19 word)
        dest = decodeRegister (extractBits 7 11 word)

decodeWidth :: MonadError DecodingError m => Word32 -> m Width
decodeWidth twoBits =
  case twoBits of
    0b00 -> pure Byte
    0b01 -> pure Half
    0b10 -> pure Word
    _ ->
      throwError $ DecodingError ("Unsupported store width: " <> show twoBits)

decodeStoreInstr :: MonadError DecodingError m => Word32 -> m MemoryInstr
decodeStoreInstr word = do
  width <- decodeWidth (extractBits 12 13 word)
  pure (STORE width (Word12 $ fromIntegral offset) src base)
  where offset4to0 = extractBits 7 11 word
        offset5to11 = extractBits 25 31 word
        offset = (offset5to11 `shiftL` 5) .|. offset4to0
        base = decodeRegister (extractBits 15 19 word)
        src = decodeRegister (extractBits 20 24 word)

decodeROpcode :: MonadError DecodingError m => Bool -> Word32 -> m ROpcode
decodeROpcode funct7 threeBits =
  case threeBits of
    0b000 -> pure (if funct7 then SUB else ADD)
    0b001 -> pure SLL
    0b010 -> pure SLT
    0b011 -> pure SLTU
    0b100 -> pure XOR
    0b101 -> pure (if funct7 then SRA else SRL)
    0b110 -> pure OR
    0b111 -> pure AND
    _ -> throwError $ DecodingError ("Unsupported register-register opcode: " <> show threeBits)

decodeRRInstr :: MonadError DecodingError m => Word32 -> m RegisterRegisterInstr
decodeRRInstr word = do
  opcode <- decodeROpcode funct7 (extractBits 12 14 word)
  pure (RInstr opcode src2 src1 dest)
  where src2 = decodeRegister (extractBits 20 24 word)
        src1 = decodeRegister (extractBits 15 19 word)
        dest = decodeRegister (extractBits 7 11 word)
        funct7 = testBit word 30

decodeIOpcode :: MonadError DecodingError m => Word32 -> m IOpcode
decodeIOpcode threeBits =
  case threeBits of
    0b000 -> pure ADDI
    0b010 -> pure SLTI
    0b011 -> pure SLTIU
    0b100 -> pure XORI
    0b110 -> pure ORI
    0b111 -> pure ANDI
    _ -> throwError $ DecodingError ("Unsupported register-immediate opcode: " <> show threeBits)

decodeShiftOpcode :: MonadError DecodingError m => Bool -> Word32 -> m ShiftOpcode
decodeShiftOpcode funct7 threeBits =
  case threeBits of
    0b001 -> pure SLLI
    0b101 -> pure (if funct7 then SRAI else SRLI)
    _ -> throwError $ DecodingError ("Unsupported shift opcode: " <> show threeBits)

isShiftOpcode :: Word32 -> Bool
isShiftOpcode word = word == 0b001 || word == 0b101

decodeRIInstr :: MonadError DecodingError m => Word32 -> m RegisterImmediateInstr
decodeRIInstr word =
  if isShiftOpcode opcode then do
    let shamt = Word5 (fromIntegral $ extractBits 20 24 word)
    shiftOpcode <- decodeShiftOpcode (testBit word 30) opcode
    pure (ShiftInstr shiftOpcode shamt src dest)
  else do let immediate = Word12 (fromIntegral $ extractBits 20 31 word)
          iOpcode <- decodeIOpcode word
          pure (IInstr iOpcode immediate src dest)
  where dest = decodeRegister (extractBits 7 11 word)
        src = decodeRegister (extractBits 15 19 word)
        opcode = extractBits 12 14 word

decodeLUIInstr :: MonadError DecodingError m => Word32 -> m RegisterImmediateInstr
decodeLUIInstr word = pure (LUI immediate dest)
  where
    dest = decodeRegister (extractBits 7 11 word)
    immediate = Word20 (extractBits 12 31 word)

decodeAUIPCInstr :: MonadError DecodingError m => Word32 -> m RegisterImmediateInstr
decodeAUIPCInstr word = pure (AUIPC immediate dest)
  where
    dest = decodeRegister (extractBits 7 11 word)
    immediate = Word20 (extractBits 12 31 word)

decodeSyncOrderingAtBit :: Int -> Word32 -> SyncOrdering
decodeSyncOrderingAtBit i word =
  SyncOrd
    (testBit word (i + 3))
    (testBit word (i + 2))
    (testBit word (i + 1))
    (testBit word i)

decodeSyncInstr :: MonadError DecodingError m => Word32 -> m SynchronizationInstr
decodeSyncInstr word =
  if testBit word 12
    then pure FENCEI
    else pure
           (FENCE
              (decodeSyncOrderingAtBit 24 word)
              (decodeSyncOrderingAtBit 20 word))

decodeRegister :: Word32 -> Register
decodeRegister reg =
  case reg of
    0b00000 -> X0
    0b00001 -> X1
    0b00010 -> X2
    0b00011 -> X3
    0b00100 -> X4
    0b00101 -> X5
    0b00110 -> X6
    0b00111 -> X7
    0b01000 -> X8
    0b01001 -> X9
    0b01010 -> X10
    0b01011 -> X11
    0b01100 -> X12
    0b01101 -> X13
    0b01110 -> X14
    0b01111 -> X15
    0b10000 -> X16
    0b10001 -> X17
    0b10010 -> X18
    0b10011 -> X19
    0b10100 -> X20
    0b10101 -> X21
    0b10110 -> X22
    0b10111 -> X23
    0b11000 -> X24
    0b11001 -> X25
    0b11010 -> X26
    0b11011 -> X27
    0b11100 -> X28
    0b11101 -> X29
    0b11110 -> X30
    0b11111 -> X31
    _ -> error "Word5 invariant violated"
