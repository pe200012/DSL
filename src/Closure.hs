{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE TypeOperators #-}

{-|
Intermediate representation of the lambda calculus.
-}
module Closure ( module Closure ) where

import           Bytecode
                 ( Chunk(..)
                 , Constant(..)
                 , Function(..)
                 , InstrArg(..)
                 , Instruction(..)
                 , Opcode(..)
                 , defaultHeader
                 , emptyMainFunction
                 , instr )

import           Control.Lens
                 ( (^.)
                 , (^?!)
                 , Iso'
                 , at
                 , from
                 , iso
                 , ix
                 , makeLenses
                 , over
                 , view )

import           Data.Text.Lazy           ( Text )
import           Data.Word                ( Word32
                                          , Word64 )

import           Debug.Trace              ( traceShow )

import           Development.Placeholders ( notImplemented )

import           Polysemy
import           Polysemy.Reader          ( Reader
                                          , asks
                                          , local
                                          , runReader )
import           Polysemy.State

data Term = CVar Text | CLet Text Term [Term] | CValue Value
    deriving ( Show
             , Eq )

newtype Closure = Closure { freeVars :: [Text] }
    deriving ( Show
             , Eq )

data Value = CInt Word64 | CBool Bool | CNil
    deriving ( Show
             , Eq )

isoValueConstant :: Iso' Value Constant
isoValueConstant = iso toConstant fromConstant
  where
    toConstant :: Value -> Constant
    toConstant (CInt i) = LUA_VNUM (fromIntegral i)
    toConstant (CBool b) = if b then LUA_VTRUE else LUA_VFALSE
    toConstant CNil = LUA_VNIL

    fromConstant :: Constant -> Value
    fromConstant (LUA_VINT i) = CInt i
    fromConstant LUA_VNIL = CNil
    fromConstant LUA_VFALSE = CBool False
    fromConstant LUA_VTRUE = CBool True
    fromConstant _ = $notImplemented

data CompileEnv = CompileEnv { _compileenvCode      :: [Instruction]
                             , _compileenvConstants :: [Constant]
                             }
    deriving ( Show
             , Eq )

emptyEnv :: CompileEnv
emptyEnv = CompileEnv { _compileenvCode = [], _compileenvConstants = [] }

makeLenses ''CompileEnv

newtype CompileVars = CompileVars { _vars :: [(Text, Word64)] }
    deriving ( Show
             , Eq )

emptyVars :: CompileVars
emptyVars = CompileVars []

makeLenses ''CompileVars

pushInstr :: Member (State CompileEnv) r => Instruction -> Sem r ()
pushInstr i = modify (over compileenvCode (i :))

return0 :: Member (State CompileEnv) r => Sem r ()
return0 = pushInstr (instr OP_RETURN0 (IABCk 0 0 0 False))

loadI :: Member (State CompileEnv) r => Word64 -> Sem r ()
loadI i = do
  top <- gets (fromIntegral . length . _compileenvConstants)
  pushInstr (instr OP_LOADK (IABx top (fromIntegral i)))

loadNil :: Member (State CompileEnv) r => Sem r ()
loadNil = do
  top <- gets (fromIntegral . length . _compileenvConstants)
  pushInstr (instr OP_LOADNIL (IABCk top top 0 False))

loadBool :: Member (State CompileEnv) r => Bool -> Sem r ()
loadBool b = do
  top <- gets (fromIntegral . length . _compileenvConstants)
  if b
      then pushInstr (instr OP_LOADTRUE (IAx top))
      else pushInstr (instr OP_LOADFALSE (IAx top))

loadValue :: Member (State CompileEnv) r => Value -> Sem r ()
-- loadValue (CInt i) = loadI i
-- loadValue (CBool b) = loadBool b
-- loadValue CNil = loadNil
loadValue c = do
  case c of
    CInt i  -> loadI i
    CBool b -> loadBool b
    CNil    -> loadNil
  modify (over compileenvConstants ((c ^. isoValueConstant) :))

lookupValue :: Member (State CompileEnv) r => Word64 -> Sem r Value
lookupValue i = do
  cs <- gets _compileenvConstants
  pure (cs ^?! ix (fromIntegral i) . from isoValueConstant)

compile :: Members '[ State CompileEnv, Reader CompileVars ] r
        => Term
        -> Sem r Value
compile (CVar v) = do
  vs <- asks _vars
  maybe $notImplemented lookupValue (lookup v vs)
compile (CLet binder rhs body) = do
  rhs' <- compile rhs
  loadValue rhs'
  currentConstIdx <- gets (fromIntegral . length . _compileenvConstants)
  xs <- local (over vars ((binder, currentConstIdx) :)) (mapM compile body)
  case xs of
    [] -> pure CNil
    _  -> pure (last xs)
compile (CValue v) = pure v
compile x = $notImplemented

compileFunc
    :: Members '[ State CompileEnv, Reader CompileVars ] r => Term -> Sem r ()
compileFunc t = do
  pushInstr (instr OP_VARARGPREP (IABCk 0 0 0 False))
  _ <- compile t
  return0

runCompile :: CompileEnv -> CompileVars -> Term -> Chunk
runCompile env vars term =
    Chunk { _header   = defaultHeader
          , _function = emptyMainFunction { _code      = reverse code
                                          , _constants = constants
                                          }
          }
  where
    (CompileEnv{_compileenvCode = code, _compileenvConstants = constants}, _) =
        run . runState env . runReader vars $ compileFunc term
