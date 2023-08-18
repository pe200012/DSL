{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
module Stmt where

import           Data.Text

import           Polysemy

import           Syntax    hiding ( Name )

import           Transpile

import           Types     hiding ( Name )

type Name = Text

data Stmt m a where
    Expr :: Expr -> Stmt m Expr

makeSem ''Stmt

runStmt :: Member (Embed Transpile) r => Sem (Stmt ': r) a -> Sem r a
runStmt = interpret $ \case
    Expr e -> undefined
