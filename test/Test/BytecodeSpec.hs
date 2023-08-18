{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.BytecodeSpec where

import           Bytecode

import           Data.Bits
import           Data.Word         ( Word32 )

import           Hedgehog
import           Hedgehog.Gen
import           Hedgehog.Range

import           System.Random     ( randomRIO )

import           Test.Syd
import           Test.Syd.Hedgehog ()

spec :: Spec
spec = do
  describe "Bytecode lens"
      $ do
        it "mast 0 0 x == 0"
            $ property
            $ do
              (x :: Word32) <- forAll
                  $ word32 (constantBounded @Word32)
              mask 0 0 x === x
        it "mask 0 len x == 0"
            $ property
            $ do
              (x :: Word32) <- forAll
                  $ word32 (constantBounded @Word32)
              mask 0 32 x === 0
        it "mask a b x .|. (x .&. (((1 .<<. b) - 1) .<<. a)) == x"
            $ property
            $ do
              (x :: Word32) <- forAll
                  $ word32 (constantBounded @Word32)
              a <- forAll
                  $ integral (linear 0 31)
              b <- forAll
                  $ integral (linear 0 31)
              mask a b x .|. (x .&. (((1 .<<. b) - 1) .<<. a)) === x
        describe "iABC :: all lens should work"
            $ do
              it "getOpcode (fromIntegral (fromEnum opcode)) == Just opcode"
                  $ property
                  $ do
                    (opcode :: Opcode) <- forAll enumBounded
                    getOpcode (fromIntegral (fromEnum opcode)) === Just opcode
              it "getOpcode consistent"
                  $ property
                  $ do
                    (a, b, c, ax, total, op) <- prepareInstruction
                    case getOpcode total of
                      Nothing  -> fail "getOpcode returned Nothing"
                      Just op' -> op' === op
              it "getA"
                  $ property
                  $ do
                    (a, b, c, ax, total, op) <- prepareInstruction
                    getA total === a
              it "getB"
                  $ property
                  $ do
                    (a, b, c, ax, total, op) <- prepareInstruction
                    getB total === b .>>. 1
              it "getk"
                  $ property
                  $ do
                    (a, b, c, ax, total, op) <- prepareInstruction
                    getk total === b .&. 0x1
              it "getC"
                  $ property
                  $ do
                    (a, b, c, ax, total, op) <- prepareInstruction
                    getC total === c
              it "getBx"
                  $ property
                  $ do
                    (a, b, c, ax, total, op) <- prepareInstruction
                    getBx total === b
              it "getsBx"
                  $ property
                  $ do
                    (a, b, c, ax, total, op) <- prepareInstruction
                    getsBx total === (b .|. (c .<<. 9)) - (2 ^ (16 :: Int))
  where
    prepareInstruction = do
      a <- forAll
          $ integral (linear 0 (2 ^ 8 - 1))
      b <- forAll
          $ integral (linear 0 (2 ^ 9 - 1))
      c <- forAll
          $ integral (linear 0 (2 ^ 8 - 1))
      opcode :: Opcode <- forAll enumBounded
      let ax    = a .|. (b .<<. 8) .|. (c .<<. 17)
          total = (ax .<<. 7) .|. fromIntegral (fromEnum opcode)
      pure (a, b, c, ax, total, opcode)


