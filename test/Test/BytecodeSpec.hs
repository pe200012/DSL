{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.BytecodeSpec where

import           Bytecode

import           Control.Arrow        ( Arrow(..) )
import           Control.Lens         ( (^.)
                                      , view )
import           Control.Monad        ( when )

import           Data.Binary          ( encode )
import           Data.Bits
import qualified Data.ByteString.Lazy as BS
import           Data.Foldable        ( for_ )
import           Data.List            ( isSuffixOf )
import qualified Data.Text            as T
import           Data.Traversable     ( for )
import           Data.Word            ( Word32
                                      , Word64 )

import           Hedgehog
import           Hedgehog.Gen
                 ( bool
                 , double
                 , enumBounded
                 , integral
                 , word32
                 , word64 )
import           Hedgehog.Range       ( constantBounded
                                      , linear )

import           System.Directory     ( getDirectoryContents )
import           System.Exit          ( ExitCode(ExitSuccess) )
import           System.FilePath      ( dropExtension
                                      , takeFileName )
import           System.IO            ( IOMode(WriteMode)
                                      , withFile )
import           System.IO.Temp       ( withSystemTempFile
                                      , withTempFile )
import           System.Process       ( readProcessWithExitCode )

import           Test.Syd
import           Test.Syd.Hedgehog    ()

import           Text.Printf          ( printf )

import           Utils

spec :: Spec
spec = do
  describe "utils test" $ do
    it "decodeVariable (variableBytes x) == Just (x, \"\")" $ property $ do
      x <- forAll $ word64 (linear 0 0x7FFFFFFF)
      decodeVariableBytes (variableBytes x) === Just (x, "")
    it "decodeIEEEDouble b . encodeIEEEDouble b == id" $ property $ do
      littleEndian <- forAll bool
      d <- forAll $ double (fromIntegral <$> constantBounded @Word64)
      decodeIEEEDouble littleEndian (encodeIEEEDouble littleEndian d) === d
  describe "Bytecode lens" $ do
    it "mast 0 0 x == 0" $ property $ do
      (x :: Word32) <- forAll $ word32 (constantBounded @Word32)
      mask 0 0 x === x
    it "mask 0 len x == 0" $ property $ do
      (x :: Word32) <- forAll $ word32 (constantBounded @Word32)
      mask 0 32 x === 0
    it "mask a b x .|. (x .&. (((1 .<<. b) - 1) .<<. a)) == x" $ property $ do
      (x :: Word32) <- forAll $ word32 (constantBounded @Word32)
      a <- forAll $ integral (linear 0 31)
      b <- forAll $ integral (linear 0 31)
      mask a b x .|. (x .&. (((1 .<<. b) - 1) .<<. a)) === x
    describe "iABC :: all lens should work" $ do
      it "getOpcode (fromIntegral (fromEnum opcode)) == Just opcode" $ property
          $ do
            (opcode :: Opcode) <- forAll enumBounded
            getOpcode (fromIntegral (fromEnum opcode)) === Just opcode
      it "check instr correctness" $ property $ do
        (opcode :: Opcode) <- forAll enumBounded
        instr opcode (IABCk 0 0 0 False) ^. _opcode === Just opcode
      it "getOpcode consistent" $ property $ do
        (a, b, c, ax, total, op) <- prepareInstruction
        getOpcode total === Just op
        Instruction total ^. _opcode === Just op
      it "get*" $ property $ do
        (a, b, c, ax, total, op) <- prepareInstruction
        getA total === a
        getB total === b .>>. 1
        getk total === b .&. 0x1
        getC total === c
        getBx total === b .|. (c .<<. 9)
        getsBx total === getBx total - (2 ^ (16 :: Int))
        getAx total === ax
        getsJ total === ax - (2 ^ (24 :: Int))
      it "lens*" $ property $ do
        (a, b, c, ax, total', op) <- prepareInstruction
        let total = Instruction total'
        view _A total === a
        view _B total === b .>>. 1
        view _k total === b .&. 0x1
        view _C total === c
        view _Bx total === b .|. (c .<<. 9)
        view _sBx total === getBx total' - (2 ^ (16 :: Int))
        view _Ax total === ax
        view _sJ total === ax - (2 ^ (24 :: Int))
  describe "Bytecode emitting" $ do
    let base = "test/Test/chunk_tests"
    scenarioDir base
        $ \file -> when (".chunk" `isSuffixOf` file) $ it "decompiles" $ do
          let name = dropExtension file
              dump = name <> ".dump"
          chunk <- read @Chunk <$> readFile file
          return $ goldenStringFile (name <> ".golden") $ do
            withFile dump WriteMode $ \handle -> do
              BS.hPut handle (encode chunk)
              -- invoke `luac -l test.dummy` to see the result
              (code, out, err)
                  <- readProcessWithExitCode "luac" [ "-l", "-l", dump ] ""
              code `shouldBe` ExitSuccess
              pure (err <> out)
    it "dummy return" $ do
      let chunk =
              Chunk { _header   = defaultHeader
                    , _function =
                          emptyMainFunction { _code =
                                                  [ instr OP_VARARGPREP
                                                          (IABCk 0 0 0 False)
                                                  , instr OP_RETURN0
                                                          (IABCk 0 0 0 False)
                                                  ]
                                            }
                    }
      withSystemTempFile "test.dummy" $ \path handle -> do
        BS.hPut handle (encode chunk)
        -- invoke `luac -l test.dummy` to see the result
        (code, out, err) <- readProcessWithExitCode "luac" [ "-l", path ] ""
        code `shouldBe` ExitSuccess
        (code, out, err) <- readProcessWithExitCode "lua" [ path ] ""
        code `shouldBe` ExitSuccess
  where
    prepareInstruction = do
      a <- forAll $ integral (linear 0 (2 ^ 8 - 1))
      b <- forAll $ integral (linear 0 (2 ^ 9 - 1))
      c <- forAll $ integral (linear 0 (2 ^ 8 - 1))
      opcode :: Opcode <- forAll enumBounded
      let ax    = a .|. (b .<<. 8) .|. (c .<<. 17)
          total = (ax .<<. 7) .|. fromIntegral (fromEnum opcode)
      pure (a, b, c, ax, total, opcode)