{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import           Data.Text

type Name = Text

newtype TVar = TV Name
    deriving newtype ( Show, Eq, Ord )

data Type = TVar TVar | TArr Type Type | TCon Name
    deriving ( Show, Eq, Ord )

data Scheme = Forall [TVar] Type
    deriving ( Show, Eq )
