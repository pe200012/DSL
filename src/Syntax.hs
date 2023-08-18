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
    deriving ( Eq
             , Ord )

instance Show Lit where
  show (Li i) = show i
  show (Lb b) = show b
  show (Ls s) = show s
  show (Lc c) = show c
  show (Lf f) = show f
  show Lu = "()"

typeName :: Lit -> Text
typeName (Li _) = "int"
typeName (Lb _) = "bool"
typeName (Ls _) = "string"
typeName (Lc _) = "char"
typeName (Lf _) = "double"
typeName Lu = "unit"

data Expr
    = Var Name
    | App Expr Expr
    | Lam Name Expr
    | Fix Expr
    | Let Name Expr Expr
    | Lit Lit
    | If Expr Expr Expr
    | BinOp Name Expr Expr
    deriving ( Show
             , Eq )

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

if_ :: Expr -> Expr -> Expr -> Expr
if_ = If

transpileExpr :: Expr -> Text
transpileExpr = cata
    $ \case
      VarF name          -> name
      AppF f x           -> f <> "(" <> x <> ")"
      LamF name body     -> "fun " <> name <> " -> " <> body
      FixF body          -> body
      LetF name val body -> "let " <> name <> " = " <> val <> " in " <> body
      LitF lit           -> pack
          $ show lit
      IfF cond t f       -> "if " <> cond <> " then " <> t <> " else " <> f
      BinOpF op l r      -> l <> " " <> op <> " " <> r

-- >>> transpileExpr example
-- "let x = 1 in let y = 2 in x"
example :: Expr
example = let_ "x"
               (Lit
                $ Li 1)
    $ let_ "y"
           (Lit
            $ Li 2)
    $ var "x"

-- >>> transpileExpr fib
-- "fun f -> fun n -> if n = 0 then 1 else if n = 1 then 1 else f(n - 1) + f(n - 2)"
fib :: Expr
fib = "f"
    .^ ("n" .^ if_ (BinOp "="
                          (var "n")
                          (Lit
                           $ Li 0))
                   (Lit
                    $ Li 1)
                   (if_ (BinOp "="
                               (var "n")
                               (Lit
                                $ Li 1))
                        (Lit
                         $ Li 1)
                        (BinOp "+"
                               (var "f" @@ BinOp "-"
                                                 (var "n")
                                                 (Lit
                                                  $ Li 1))
                               (var "f" @@ BinOp "-"
                                                 (var "n")
                                                 (Lit
                                                  $ Li 2)))))

