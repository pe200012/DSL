-- | transpile to OCaml code
module Transpile where

import           Data.Text as Text

import           Syntax

-- | dummy, just add newlines
prettyprint :: [Text] -> Text
prettyprint = Text.unlines

newtype Transpile a = Transpile { runTranspile :: [a] }
    deriving ( Show, Eq )
