{-|
Intermediate representation of the lambda calculus.
-}
module Closure where

import           Data.Text.Lazy ( Text )

data Term
    = CVar Text
    | CApp Term Term
    | CLet Text Term Term
    | CClosure Text Term
    | CSeq Term Term
    deriving ( Show
             , Eq )

newtype Closure = Closure { freeVars :: [Text] }
    deriving ( Show
             , Eq )