
module Env where

import           Data.Map  as Map
import           Data.Text

import           Types     ( Scheme )

type Name = Text

newtype TypeEnv = TypeEnv { runTypeEnv :: Map Name Scheme }

empty :: TypeEnv
empty = TypeEnv Map.empty

extend :: TypeEnv -> (Name, Scheme) -> TypeEnv
extend (TypeEnv env) (x, s) = TypeEnv
    $ Map.insert x s env

lookup :: Name -> TypeEnv -> Maybe Scheme
lookup x (TypeEnv env) = Map.lookup x env

merge :: TypeEnv -> TypeEnv -> TypeEnv
merge (TypeEnv env1) (TypeEnv env2) = TypeEnv
    $ Map.union env1 env2

remove :: Name -> TypeEnv -> TypeEnv
remove x (TypeEnv env) = TypeEnv
    $ Map.delete x env

mergeMany :: [TypeEnv] -> TypeEnv
mergeMany = Prelude.foldr merge Env.empty

singleton :: Name -> Scheme -> TypeEnv
singleton x s = TypeEnv
    $ Map.singleton x s

keys :: TypeEnv -> [Name]
keys (TypeEnv env) = Map.keys env

fromList :: [(Name, Scheme)] -> TypeEnv
fromList = TypeEnv . Map.fromList

toList :: TypeEnv -> [(Name, Scheme)]
toList = Map.toList . runTypeEnv

instance Semigroup TypeEnv where
  (<>) = merge

instance Monoid TypeEnv where
  mempty  = Env.empty

  mappend = (<>)
