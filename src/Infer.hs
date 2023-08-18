{-# LANGUAGE TupleSections #-}

module Infer where

import           Control.Applicative   ( Applicative(..) )
import           Control.Monad.Except
import           Control.Monad.RWS

import           Data.Functor.Foldable
import           Data.List             ( nub )
import           Data.Map              as Map
import           Data.Set              as Set
import           Data.Text             ( Text, pack )

import           Env                   hiding ( Name )

import           Syntax                ( Expr(..), ExprF(..), typeName )

import           Types

type Infer a = RWST TypeEnv [Constraint] InferState (Except TypeError) a

type Constraint = (Type, Type)

newtype InferState = InferState { count :: Int }

type Unifier = (Subst, [Constraint])

emptyUnifier :: Unifier
emptyUnifier = (Subst Map.empty, [])

type Solve a = Except TypeError a

data TypeError = UnificationFail Type Type
               | InfiniteType TVar Type
               | UnboundVariable Text
               | Ambigious [Constraint]
               | UnificationMismatch [Type] [Type]
    deriving ( Show, Eq )

newtype Subst = Subst { runSubst :: Map TVar Type }
    deriving ( Show, Eq )

emptySubst :: Subst
emptySubst = Subst Map.empty

class Substitutable a where
    apply :: Subst -> a -> a
    ftv :: a -> Set TVar

instance Substitutable Type where
    apply s t@(TVar a) = Map.findWithDefault t a (runSubst s)
    apply s (TArr t1 t2) = TArr (apply s t1) (apply s t2)
    apply _ t = t

    ftv (TVar a) = Set.singleton a
    ftv (TArr t1 t2) = ftv t1 `Set.union` ftv t2
    ftv _ = Set.empty

instance Substitutable Scheme where
    apply s (Forall as t) = Forall as $ apply s' t
      where
        s' = Subst $ Prelude.foldr Map.delete (runSubst s) as

    ftv (Forall as t) = ftv t `Set.difference` Set.fromList as

instance Substitutable Constraint where
    apply s (t1, t2) = (apply s t1, apply s t2)

    ftv (t1, t2) = ftv t1 `Set.union` ftv t2

instance (Substitutable a) => Substitutable [a] where
    apply = Prelude.map . apply

    ftv   = Prelude.foldr (Set.union . ftv) Set.empty

instance Substitutable TypeEnv where
    apply s (TypeEnv env) = TypeEnv $ Map.map (apply s) env

    ftv (TypeEnv env) = ftv $ Map.elems env

fresh :: RWST TypeEnv [Constraint] InferState (Except TypeError) Type
fresh = do
    s <- get
    put s { count = count s + 1 }
    return $ TVar $ TV $ "t" <> pack (show (count s))

instantiate :: Scheme -> Infer Type
instantiate (Forall as t) = do
    as' <- mapM (const fresh) as
    let s = Subst $ Map.fromList $ Prelude.zip as as'
    return $ apply s t

generalize :: TypeEnv -> Type -> Scheme
generalize env t = Forall as t
  where
    as = Set.toList $ ftv t `Set.difference` ftv env

runInfer :: TypeEnv -> Infer Type -> Either TypeError (Type, [Constraint])
runInfer env m = runExcept $ evalRWST m env (InferState 0)

inferExpr :: TypeEnv -> Expr -> Either TypeError Scheme
inferExpr env expr = do
    (ty, cs) <- runInfer env (infer expr)
    subst <- runSolve cs
    return $ closeOver $ apply subst ty

check :: TypeEnv -> Infer Type -> Either TypeError Scheme
check env m = do
    (ty, cs) <- runInfer env m
    subst <- runSolve cs
    return $ closeOver $ apply subst ty

closeOver :: Type -> Scheme
closeOver = normalize . generalize Env.empty

normalize :: Scheme -> Scheme
normalize (Forall _ body) = Forall (snd <$> tvs) (checking body)
  where
    tvs = zip (nub (fv body)) (TV . pack <$> letters)

    letters = [ 1 .. ] >>= flip replicateM [ 'a' .. 'z' ]

    fv = Set.toList . ftv

    checking (TCon a) = TCon a
    checking (TArr t1 t2) = TArr (checking t1) (checking t2)
    checking (TVar t) = case Prelude.lookup t tvs of
        Nothing -> error "type variable not in signature"
        Just x  -> TVar x

runSolve :: [Constraint] -> Either TypeError Subst
runSolve = runExcept . solver . (emptySubst, )

uni :: Type -> Type -> Infer ()
uni t1 t2 = tell [ (t1, t2) ]

inEnv :: (Name, Scheme) -> Infer a -> Infer a
inEnv (x, sc) m = do
    let scope e = e `extend` (x, sc)
    local scope m

infer :: Expr -> Infer Type
infer = cataA $ \case
    LitF l         -> return $ TCon (typeName l)
    VarF x         -> do
        env <- ask
        case Env.lookup x env of
            Nothing -> throwError $ UnboundVariable x
            Just s  -> instantiate s
    AppF e1 e2     -> do
        tv <- fresh
        t1 <- e1
        t2 <- e2
        uni t1 (TArr t2 tv)
        return tv
    LamF x e       -> do
        tv <- fresh
        t <- inEnv (x, Forall [] tv) e
        return $ TArr tv t
    LetF x e1 e2   -> do
        env <- ask
        t1 <- e1
        let sc = generalize env t1
        inEnv (x, sc) e2
    FixF e         -> do
        tv <- fresh
        t <- e
        uni (tv `TArr` tv) t
        return tv
    IfF cond e1 e2 -> do
        t1 <- cond
        t2 <- e1
        t3 <- e2
        uni t1 (TCon "Bool")
        uni t2 t3
        return t2

bind :: TVar -> Type -> Solve Unifier
bind a t
    | t == TVar a = return emptyUnifier
    | a `Set.member` ftv t = throwError $ InfiniteType a t
    | otherwise = return (Subst $ Map.singleton a t, [])

unify :: Type -> Type -> Solve Unifier
unify t1 t2
    | t1 == t2 = return emptyUnifier
unify (TVar v) t = v `bind` t
unify t (TVar v) = v `bind` t
unify (TArr t1 t2) (TArr t3 t4) = unifyMany [ t1, t2 ] [ t3, t4 ]
unify t1 t2 = throwError $ UnificationFail t1 t2

compose :: Subst -> Subst -> Subst
compose s1 s2 = Subst $ Map.map (apply s1) (runSubst s2) `Map.union` runSubst s1

unifyMany :: [Type] -> [Type] -> Solve Unifier
unifyMany [] [] = return emptyUnifier
unifyMany (t1 : ts1) (t2 : ts2) = do
    (s1, cs1) <- unify t1 t2
    (s2, cs2) <- unifyMany (apply s1 ts1) (apply s1 ts2)
    return (s2 `Infer.compose` s1, cs1 ++ cs2)
unifyMany t1 t2 = throwError $ UnificationMismatch t1 t2

solver :: Unifier -> Solve Subst
solver (su, cs) = do
    case cs of
        [] -> return su
        ((t1, t2) : cs0) -> do
            (su1, cs1) <- unify t1 t2
            solver (su1 `Infer.compose` su, cs1 ++ apply su1 cs0)

infixr 2 @@

(@@) :: Type -> Type -> Infer Type
t1 @@ t2 = do
    tv <- fresh
    uni (t2 `TArr` tv) t1
    return tv

infixr 1 .^

(.^) :: Name -> (Type -> Infer Type) -> Infer Type
v .^ t = do
    tv <- fresh
    u <- inEnv (v, Forall [] tv) (t tv)
    return $ TArr tv u

fix :: Type -> Infer Type
fix t = do
    tv <- fresh
    uni (tv `TArr` tv) t
    return tv

let_ :: Infer Type -> (Type -> Infer Type) -> Infer Type
let_ e t = do
    env <- ask
    t1 <- e
    let sc = generalize env t1
    t t1

let' :: (Type -> Infer Type) -> Infer Type
let' t = do
    env <- ask
    tv <- fresh
    let sc = generalize env tv
    t tv

if_ :: Type -> Infer Type -> Infer Type -> Infer Type
if_ t1 t2 t3 = do
    uni t1 (TCon "Bool")
    t2' <- t2
    t3' <- t3
    uni t2' t3'
    return t2'

-- >>> example
-- Right (Forall ["a"] (TArr (TCon "Bool") (TVar "a")))
example = check Env.empty $ do
    t <- "self" .^ \self -> "i" .^ \i -> if_ i (self @@ i) (self @@ i)
    Infer.fix t
