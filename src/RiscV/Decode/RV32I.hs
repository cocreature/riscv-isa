{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE FlexibleContexts #-}
module RiscV.Decode.RV32I
  ( decodeInstr
  , DecodingError(..)
  ) where

import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Bits
import Data.Monoid
import Data.Word
import RiscV.Internal.Util
import RiscV.RV32I

data GetState = GetState
  { pos :: !Int
  , word32 :: !Word32
  }

type GetT m a = StateT GetState m a

runGetT :: Monad m => Word32 -> GetT m a -> m a
runGetT w g = evalStateT g (GetState 31 w)

getNextBits :: Monad m => Int -> GetT m Word32
getNextBits i = do
  GetState { pos = pos', word32 = w } <- get
  put (GetState (pos' - i) w)
  pure (extractBits (pos' - i + 1) pos' w)

data DecodingError = DecodingError
  { errorMsg :: !String
  } deriving (Show, Eq, Ord)

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
    opCode = bitsFromTo 0 6 word

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
decodeBranchInstr word =
  runGetT word $ do
    imm12 <- getNextBits 1
    imm10to5 <- getNextBits 6
    src2 <- decodeRegister <$> getNextBits 5
    src1 <- decodeRegister <$> getNextBits 5
    cond <- decodeBranchCond =<< getNextBits 3
    imm4to1 <- getNextBits 4
    imm11 <- getNextBits 1
    let imm =
          (imm12 `shiftL` 11) .|. (imm11 `shiftL` 10) .|. (imm10to5 `shiftL` 4) .|.
          imm4to1
    pure $ Branch (Word12 $ fromIntegral imm) cond src2 src1

decodeCSROrEnvInstr :: MonadError DecodingError m => Word32 -> m Instr
decodeCSROrEnvInstr word =
  case opCode of
    0b000 -> EnvironmentInstr <$> decodeEnvInstr word
    _ -> CSRInstr <$> decodeCSRInstr word
  where opCode = extractBits 12 14 word

decodeEnvInstr :: Applicative m => Word32 -> m EnvironmentInstr
decodeEnvInstr word =
  if testBit word 20
    then pure EBREAK
    else pure ECALL

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
    then let zimm = Word5 . fromIntegral $ extractBits 15 19 word
         in pure $ CSRIInstr csrType csr zimm dest
    else let src = decodeRegister (extractBits 15 19 word)
         in pure $ CSRRInstr csrType csr src dest
  where
    csr = CSRRegister . Word12 . fromIntegral $ extractBits 20 31 word
    dest = decodeRegister (extractBits 7 11 word)

decodeJALInstr :: MonadError DecodingError m => Word32 -> m JumpInstr
decodeJALInstr word =
  runGetT word $ do
    imm20 <- getNextBits 1
    imm10to1 <- getNextBits 10
    imm11 <- getNextBits 1
    imm19to12 <- getNextBits 8
    dest <- decodeRegister <$> getNextBits 5
    let imm =
          (imm20 `shiftL` 19) .|. (imm19to12 `shiftL` 11) .|.
          (imm11 `shiftL` 10) .|.
          imm10to1
    pure (JAL (Word20 imm) dest)

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
decodeStoreInstr word =
  runGetT word $ do
    offset5to11 <- getNextBits 7
    src <- decodeRegister <$> getNextBits 5
    base <- decodeRegister <$> getNextBits 5
    width <- decodeWidth =<< getNextBits 3
    offset4to0 <- getNextBits 5
    let offset = (offset5to11 `shiftL` 5) .|. offset4to0
    pure (STORE width (Word12 $ fromIntegral offset) src base)

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
decodeRRInstr word = runGetT word $ do
  let funct7 = testBit word 30
  _ <- getNextBits 7
  src2 <- decodeRegister <$> getNextBits 5
  src1 <- decodeRegister <$> getNextBits 5
  opcode <- decodeROpcode funct7 =<< getNextBits 3
  dest <- decodeRegister <$> getNextBits 5
  pure (RInstr opcode src2 src1 dest)

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
          iOpcode <- decodeIOpcode opcode
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
