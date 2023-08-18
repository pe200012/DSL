{-# LANGUAGE BinaryLiterals #-}

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Bytecode where

import           Control.Lens ( Lens'
                              , lens
                              , makeLenses
                              , over
                              , to
                              , view )

import           Data.Bits    ( (.<<.)
                              , (.>>.)
                              , Bits(..) )
import           Data.Word    ( Word32 )

import           Text.Printf  ( printf )

{-
Ref: https://www.lua.org/source/5.4/lopcodes.h.html
Instructions can have the following formats:

        3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0
        1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
iABC          C(8)     |      B(8)     |k|     A(8)      |   Op(7)     |
iABx                Bx(17)               |     A(8)      |   Op(7)     |
iAsBx              sBx (signed)(17)      |     A(8)      |   Op(7)     |
iAx                           Ax(25)                     |   Op(7)     |
isJ                           sJ (signed)(25)            |   Op(7)     |

  A signed argument is represented in excess K: the represented value is
  the written unsigned value minus K, where K is half the maximum for the
  corresponding unsigned argument.
-}
newtype Instruction = Instruction { _payload :: Word32 }
    deriving newtype ( Eq
                     , Num )

instance Show Instruction where
  show = printf "%#.8x" . _payload

makeLenses ''Instruction

-- | clear out certain bits in a word
{-# INLINE mask #-}
mask :: Int -> Int -> Word32 -> Word32
mask start len w = w .&. complement (((1 .<<. len) - 1) .<<. start)

{-# INLINE blit #-}
blit :: Int -> Int -> Word32 -> Word32 -> Word32
blit start len src dst = mask start len dst
    .|. ((src .&. ((1 .<<. len) - 1)) .<<. start)

{-# INLINE getA #-}
getA :: Word32 -> Word32
getA w = (w .>>. 7) .&. 0xFF

{-# INLINE getB #-}
getB :: Word32 -> Word32
getB w = (w .>>. 16) .&. 0xFF

{-# INLINE getk #-}
getk :: Word32 -> Word32
getk w = (w .>>. 15) .&. 0x1

{-# INLINE getC #-}
getC :: Word32 -> Word32
getC w = (w .>>. 24) .&. 0xFF

{-# INLINE getsC #-}
getsC :: Word32 -> Word32
getsC w = (mask 0 24 w .>>. 24) - (2 ^ (7 :: Int))

{-# INLINE getBx #-}
getBx :: Word32 -> Word32
getBx w = (w .>>. 15) .&. 0x1FF

{-# INLINE getsBx #-}
getsBx :: Word32 -> Word32
getsBx w = (mask 0 15 w .>>. 15) - (2 ^ (16 :: Int))

{-# INLINE getAx #-}
getAx :: Word32 -> Word32
getAx w = w .>>. 7

{-# INLINE getsJ #-}
getsJ :: Word32 -> Word32
getsJ w = getAx w - (2 ^ (24 :: Int))

{-# INLINE getOpcode #-}
getOpcode :: Word32 -> Maybe Opcode
getOpcode w =
    if w' < fromEnum (minBound :: Opcode) || w' > fromEnum (maxBound :: Opcode)
    then Nothing
    else Just (toEnum w')
  where
    w' = fromIntegral (w .&. 0x7F)

{-

>>> total :: Instruction = 5

>>> view _opcode total
Just OP_LOADFALSE

>>> set _opcode (Just OP_EXTRAARG) total
0x00000052

-}
_opcode :: Lens' Instruction (Maybe Opcode)
_opcode =
    lens (view (payload . to getOpcode))
         (\i opcode -> case opcode of
            Nothing      -> over payload (.|. 0x7F) i
            Just opcode' ->
                over payload (blit 0 7 (fromIntegral (fromEnum opcode'))) i)

_A :: Lens' Instruction Word32
_A = lens (view (payload . to getA)) (\i a -> over payload (blit 7 8 a) i)

_B :: Lens' Instruction Word32
_B = lens (view (payload . to getB)) (\i b -> over payload (blit 16 8 b) i)

_k :: Lens' Instruction Word32
_k = lens (view (payload . to getk)) (\i k -> over payload (blit 15 1 k) i)

_C :: Lens' Instruction Word32
_C = lens (view (payload . to getC)) (\i c -> over payload (blit 24 8 c) i)

_sC :: Lens' Instruction Word32
_sC = lens (view (payload . to getsC)) (\i sC -> over payload (blit 24 8 sC) i)

_Bx :: Lens' Instruction Word32
_Bx = lens (view (payload . to getBx)) (\i bx -> over payload (blit 15 9 bx) i)

_sBx :: Lens' Instruction Word32
_sBx = lens (view (payload . to getsBx))
            (\i sBx -> over payload (blit 15 9 sBx) i)

_Ax :: Lens' Instruction Word32
_Ax = lens (view (payload . to getAx)) (\i ax -> over payload (blit 7 25 ax) i)

_sJ :: Lens' Instruction Word32
_sJ = lens (view (payload . to getsJ)) (\i sJ -> over payload (blit 7 25 sJ) i)

-- | Lua VM opcodes
data Opcode
    = OP_MOVE  -- ^       A B     R[A] := R[B]
    | OP_LOADI  -- ^      A sBx   R[A] := sBx
    | OP_LOADF  -- ^      A sBx   R[A] := (lua_Number)sBx
    | OP_LOADK  -- ^      A Bx    R[A] := K[Bx]
    | OP_LOADKX  -- ^     A       R[A] := K[extra arg]
    | OP_LOADFALSE  -- ^  A       R[A] := false
    | OP_LFALSESKIP   -- ^A       R[A] := false; pc++     (*)
    | OP_LOADTRUE  -- ^   A       R[A] := true
    | OP_LOADNIL  -- ^    A B     R[A], R[A+1], ..., R[A+B] := nil
    | OP_GETUPVAL  -- ^   A B     R[A] := UpValue[B]
    | OP_SETUPVAL  -- ^   A B     UpValue[B] := R[A]
    | OP_GETTABUP  -- ^   A B C   R[A] := UpValue[B][K[C]:string]
    | OP_GETTABLE  -- ^   A B C   R[A] := R[B][R[C]]
    | OP_GETI  -- ^       A B C   R[A] := R[B][C]
    | OP_GETFIELD  -- ^   A B C   R[A] := R[B][K[C]:string]
    | OP_SETTABUP  -- ^   A B C   UpValue[A][K[B]:string] := RK(C)
    | OP_SETTABLE  -- ^   A B C   R[A][R[B]] := RK(C)
    | OP_SETI  -- ^       A B C   R[A][B] := RK(C)
    | OP_SETFIELD  -- ^   A B C   R[A][K[B]:string] := RK(C)
    | OP_NEWTABLE  -- ^   A B C k R[A] := {}
    | OP_SELF  -- ^       A B C   R[A+1] := R[B]; R[A] := R[B][RK(C):string]
    | OP_ADDI  -- ^       A B sC  R[A] := R[B] + sC
    | OP_ADDK  -- ^       A B C   R[A] := R[B] + K[C]:number
    | OP_SUBK  -- ^       A B C   R[A] := R[B] - K[C]:number
    | OP_MULK   -- ^      A B C   R[A] := R[B] * K[C]:number
    | OP_MODK  -- ^       A B C   R[A] := R[B] % K[C]:number
    | OP_POWK  -- ^       A B C   R[A] := R[B] ^ K[C]:number
    | OP_DIVK  -- ^       A B C   R[A] := R[B] / K[C]:number
    | OP_IDIVK  -- ^      A B C   R[A] := R[B] // K[C]:number
    | OP_BANDK  -- ^      A B C   R[A] := R[B] & K[C]:integer
    | OP_BORK  -- ^       A B C   R[A] := R[B] | K[C]:integer
    | OP_BXORK  -- ^      A B C   R[A] := R[B] ~ K[C]:integer
    | OP_SHRI  -- ^       A B sC  R[A] := R[B] >> sC
    | OP_SHLI  -- ^       A B sC  R[A] := sC << R[B]
    | OP_ADD  -- ^        A B C   R[A] := R[B] + R[C]
    | OP_SUB  -- ^        A B C   R[A] := R[B] - R[C]
    | OP_MUL  -- ^       A B C   R[A] := R[B] * R[C]
    | OP_MOD  -- ^        A B C   R[A] := R[B] % R[C]
    | OP_POW  -- ^        A B C   R[A] := R[B] ^ R[C]
    | OP_DIV  -- ^        A B C   R[A] := R[B] / R[C]
    | OP_IDIV  -- ^       A B C   R[A] := R[B] // R[C]
    | OP_BAND  -- ^       A B C   R[A] := R[B] & R[C]
    | OP_BOR  -- ^        A B C   R[A] := R[B] | R[C]
    | OP_BXOR  -- ^       A B C   R[A] := R[B] ~ R[C]
    | OP_SHL  -- ^        A B C   R[A] := R[B] << R[C]
    | OP_SHR  -- ^        A B C   R[A] := R[B] >> R[C]
    | OP_MMBIN  -- ^      A B C   call C metamethod over R[A] and R[B]    (*)
    | OP_MMBINI  -- ^     A sB C k        call C metamethod over R[A] and sB
    | OP_MMBINK  -- ^     A B C k         call C metamethod over R[A] and K[B]
    | OP_UNM  -- ^        A B     R[A] := -R[B]
    | OP_BNOT  -- ^       A B     R[A] := ~R[B]
    | OP_NOT  -- ^        A B     R[A] := not R[B]
    | OP_LEN  -- ^        A B     R[A] := #R[B] (length operator)
    | OP_CONCAT  -- ^     A B     R[A] := R[A].. ... ..R[A + B - 1]
    | OP_CLOSE  -- ^      A       close all upvalues >= R[A]
    | OP_TBC  -- ^        A       mark variable A "to be closed"
    | OP_JMP  -- ^        sJ      pc += sJ
    | OP_EQ  -- ^         A B k   if ((R[A] == R[B]) ~= k) then pc++
    | OP_LT  -- ^         A B k   if ((R[A] <  R[B]) ~= k) then pc++
    | OP_LE  -- ^         A B k   if ((R[A] <= R[B]) ~= k) then pc++
    | OP_EQK  -- ^        A B k   if ((R[A] == K[B]) ~= k) then pc++
    | OP_EQI  -- ^        A sB k  if ((R[A] == sB) ~= k) then pc++
    | OP_LTI  -- ^        A sB k  if ((R[A] < sB) ~= k) then pc++
    | OP_LEI  -- ^        A sB k  if ((R[A] <= sB) ~= k) then pc++
    | OP_GTI  -- ^        A sB k  if ((R[A] > sB) ~= k) then pc++
    | OP_GEI  -- ^        A sB k  if ((R[A] >= sB) ~= k) then pc++
    | OP_TEST  -- ^       A k     if (not R[A] == k) then pc++
    | OP_TESTSET  -- ^    A B k   if (not R[B] == k) then pc++ else R[A] := R[B] (*)
    | OP_CALL  -- ^       A B C   R[A], ... ,R[A+C-2] := R[A](R[A+1], ... ,R[A+B-1])
    | OP_TAILCALL  -- ^   A B C k return R[A](R[A+1], ... ,R[A+B-1])
    | OP_RETURN  -- ^     A B C k return R[A], ... ,R[A+B-2]      (see note)
    | OP_RETURN0  -- ^            return
    | OP_RETURN1  -- ^    A       return R[A]
    | OP_FORLOOP  -- ^    A Bx    update counters; if loop continues then pc-=Bx;
    | OP_FORPREP  -- ^   A Bx    <check values and prepare counters>;  if not to run then pc+=Bx+1;
    | OP_TFORPREP  -- ^   A Bx    create upvalue for R[A + 3]; pc+=Bx
    | OP_TFORCALL  -- ^   A C     R[A+4], ... ,R[A+3+C] := R[A](R[A+1], R[A+2]);
    | OP_TFORLOOP  -- ^   A Bx    if R[A+2] ~= nil then { R[A]=R[A+2]; pc -= Bx }
    | OP_SETLIST  -- ^    A B C k R[A][C+i] := R[A+i], 1 <= i <= B
    | OP_CLOSURE  -- ^    A Bx    R[A] := closure(KPROTO[Bx])
    | OP_VARARG  -- ^     A C     R[A], R[A+1], ..., R[A+C-2] = vararg
    | OP_VARARGPREP  -- ^ A       (adjust vararg parameters)
    | OP_EXTRAARG  -- ^   Ax      extra (larger) argument for previous opcode
    deriving ( Show
             , Eq
             , Enum
             , Bounded )

{-- |
>>> numOpcodes
83
-}
numOpcodes :: Int
numOpcodes = fromEnum (maxBound :: Opcode) + 1
