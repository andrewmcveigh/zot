{-# OPTIONS_GHC -fno-warn-orphans #-}

module Check where

import Core hiding (null)
import Check.Types as T
import Syntax.Types as S

import Data.Map.Lazy (findWithDefault)
import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set

instance HasFTV Type where
  ftv (T.Con _)   = Set.empty
  ftv (T.Var x)   = Set.singleton x
  ftv (T.Arr a b) = ftv a <> ftv b
  -- ftv (T.App x y) = ftv x <> ftv y

instance HasFTV Scheme where
  ftv (Forall as t) = ftv t `Set.difference` Set.fromList as

instance HasFTV a => HasFTV [a] where
  ftv = foldr (Set.union . ftv) Set.empty

instance HasFTV Env where
  ftv (Env env) = ftv $ Map.elems env

instance Substitute Type where
  sub _ (T.Con c)   = Con c
  sub s (T.Var x)   = findWithDefault (Var x) x (unSub s)
  sub s (T.Arr a b) = Arr (sub s a) (sub s b)
  -- sub s (T.App x y) = T.App (sub s x) (sub s y)

instance Substitute Scheme where
  sub (Sub s) (Forall as t) = Forall as $ sub s' t
    where s' = Sub $ foldr Map.delete s as

instance Substitute Env where
  sub s (Env m) = Env $ Map.map (sub s) m

comp :: Sub -> Sub -> Sub
comp s@(Sub s1) (Sub s2) = Sub $ Map.map (sub s) s2 `Map.union` s1

occurs :: HasFTV a => Name -> a -> Bool
occurs a t = Set.member a (ftv t)

bind :: Name -> Type -> Infer Sub
bind a t
  | t == Var a = pure null
  | occurs a t = throwError $ InfiniteType a t
  | otherwise  = pure $ singleton a t

unify :: Unification
unify (Arr a b) (Arr a' b') = do
  s1 <- unify a a'
  s2 <- sub s1 b `unify` sub s1 b'
  pure $ comp s2 s1
unify (Var a) t = bind a t
unify t (Var a) = bind a t
unify (Con a) (Con b) | a == b = return null
unify a b = throwError $ UnificationFailure a b

fresh :: Infer Type
fresh = gets var <* modify (+1) where
  var i = Var . Name $ "a" <> pack (show i)

instantiate :: Scheme -> Infer Type
instantiate (Forall as t) =
  flip sub t . Sub . Map.fromList . zip as <$> mapM (const fresh) as

generalize :: Env -> Type -> Scheme
generalize env t = Forall as t where
  as = Set.toList $ ftv t `Set.difference` ftv env

resolve :: Name -> Infer Scheme
resolve a = do
  Env env <- ask
  case Map.lookup a env of
    Just t  -> pure t
    Nothing -> throwError $ ResolutionFailure a

inEnv :: Name -> Scheme -> Infer a -> Infer a
inEnv x sc m =
  let scope (Env e) = Env $ Map.insert x sc e in
    local scope m

infer :: Term -> Infer (Sub, Type)

infer Unit = pure (null, unit)

infer (Lit a) = pure (null, toType a)

infer (Sym a) = resolve a >>= instantiate <&> (,) null

infer (S.App a b) = do
  tv <- fresh
  (s1, t1) <- infer a
  (s2, t2) <- local (sub s1) $ infer b
  s3 <- sub s2 t1 `unify` Arr t2 tv
  pure (s3 `comp` s2 `comp` s1, sub s3 tv)

infer (Lam (Lambda x e)) = do
  tv <- fresh
  (s1, t1) <- inEnv x (Forall [] tv) $ infer e
  pure (s1, Arr (sub s1 tv) t1)

runInfer :: Env -> Term -> Either TypeError Type
runInfer env term =
  case runExcept (runStateT (runReaderT (infer term) env) 0) of
    Right ((_, t), _) -> Right t
    Left e            -> Left e
