module RiscV.RV32ISpec where

import Control.Monad
import Test.Hspec
import Test.QuickCheck

import RiscV.Decode.RV32I
import RiscV.Encode.RV32I

spec :: Spec
spec = do
  describe "Instruction Serialization" $ do
    it "roundtrips via serialization & deserialization" $ do
      property $ \i -> decodeInstr (encodeInstr i) == Right i
