{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE TypeFamilies #-}

module Syntax where

import           Data.Functor.Foldable
import           Data.Functor.Foldable.TH
import           Data.Text

type Name = Text

data Lit = Li Int | Lb Bool | Ls String | Lc Char | Lf Double | Lu
    deriving ( Show, Eq, Ord )

typeName :: Lit -> Text
typeName (Li _) = "int"
typeName (Lb _) = "bool"
typeName (Ls _) = "string"
typeName (Lc _) = "char"
typeName (Lf _) = "double"
typeName Lu = "unit"

data Expr = Var Name
          | App Expr Expr
          | Lam Name Expr
          | Fix Expr
          | Let Name Expr Expr
          | Lit Lit
          | If Expr Expr Expr
    deriving ( Show, Eq )

makeBaseFunctor ''Expr

var :: Name -> Expr
var = Var

infixr 2 @@

(@@) :: Expr -> Expr -> Expr
(@@) = App

infixr 1 .^

(.^) :: Name -> Expr -> Expr
(.^) = Lam

fix :: Expr -> Expr
fix = Fix

let_ :: Name -> Expr -> Expr -> Expr
let_ = Let

class Lifting a where
    (↑) :: a -> Expr

instance Lifting Int where
    (↑) = Lit . Li

instance Lifting Bool where
    (↑) = Lit . Lb

instance Lifting String where
    (↑) = Lit . Ls

instance Lifting Char where
    (↑) = Lit . Lc

instance Lifting Double where
    (↑) = Lit . Lf

instance Lifting () where
    (↑) = Lit . const Lu

if_ :: Expr -> Expr -> Expr -> Expr
if_ = If
