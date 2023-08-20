{-# LANGUAGE MagicHash #-}

module Utils
    ( decodeIEEEDouble
    , encodeIEEEDouble
    , variableBytes
    , decodeVariableBytes
    , putVariableBytes
    , getVariableBytes
    , putLuaString
    , getLuaString
    ) where

import           Data.Binary.Get
                 ( Get
                 , getLazyByteString
                 , getWord64be
                 , getWord64le
                 , getWord8
                 , runGet )
import           Data.Binary.Put      ( Put
                                      , putLazyByteString
                                      , putWord64be
                                      , putWord64le
                                      , runPut )
import           Data.Bits
import           Data.ByteString.Lazy ( ByteString
                                      , pack )
import qualified Data.ByteString.Lazy as BS
import           Data.Word            ( Word64 )

import           GHC.Exts             ( unsafeCoerce# )
import           GHC.Types            ( Double(D#) )
import           GHC.Word             ( Word64(W64#) )

decodeIEEEDouble :: Bool -> ByteString -> Double
decodeIEEEDouble littleEndian = word64ToDouble . bytestringToWord64 littleEndian

encodeIEEEDouble :: Bool -> Double -> ByteString
encodeIEEEDouble littleEndian = word64ToBytestring littleEndian . doubleToWord64

word64ToDouble :: Word64 -> Double
word64ToDouble (W64# x) = D# (unsafeCoerce# x)

doubleToWord64 :: Double -> Word64
doubleToWord64 (D# x) = W64# (unsafeCoerce# x)

bytestringToWord64 :: Bool -> ByteString -> Word64
bytestringToWord64 littleEndian =
    runGet $ if littleEndian then getWord64le else getWord64be

word64ToBytestring :: Bool -> Word64 -> ByteString
word64ToBytestring littleEndian =
    runPut . if littleEndian then putWord64le else putWord64be

-- | encode into Lua 5.4's variable integer format
--
-- Data is grouped into 7-bit chunks, with the most significant bit of each
-- chunk set to 0 if there are more chunks to follow, or 1 if it is the last
-- chunk.
variableBytes :: (Bits a, Integral a) => a -> ByteString
variableBytes a = go [ lastByte ] (a .>>. 7)
  where
    lastByte = fromIntegral ((a .&. 0x7f) .|. 0x80)

    go acc x
        | x == 0 = pack acc
        | otherwise = go (fromIntegral (x .&. 0x7f) : acc) (x .>>. 7)

putVariableBytes :: (Bits a, Integral a) => a -> Put
putVariableBytes = putLazyByteString . variableBytes

-- | decode from Lua 5.4's variable integer format
--
-- Decode until the most significant bit of a byte is 1. Return Nothing
-- if EOF is reached before that.
decodeVariableBytes :: ByteString -> Maybe (Word64, ByteString)
decodeVariableBytes = go 0 0
  where
    go acc idx bs
        | idx >= min 9 (BS.length bs) = Nothing
        | otherwise = let byte = BS.index bs idx
                          acc' = acc .<<. 7 .|. fromIntegral (byte .&. 0x7f)
                      in
                          if testBit byte 7
                          then Just (acc', BS.drop (idx + 1) bs)
                          else go acc' (idx + 1) bs

getVariableBytes :: Get Word64
getVariableBytes = do
  byte <- getWord8
  if testBit byte 7
      then return $ fromIntegral (byte .&. 0x7f)
      else do
        rest <- getVariableBytes
        return $ rest .<<. 7 .|. fromIntegral byte

putLuaString :: ByteString -> Put
putLuaString bs = do
  putVariableBytes (BS.length bs + 1)
  putLazyByteString bs

getLuaString :: Get ByteString
getLuaString = do
  len <- getVariableBytes
  getLazyByteString (fromIntegral len - 1)
