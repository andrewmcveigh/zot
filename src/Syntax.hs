{-# LANGUAGE UndecidableInstances #-}

module Syntax where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, findWithDefault)
import Data.Functor.Foldable
import Control.Monad.Except
import Control.Monad.RWS.Lazy
import Control.Monad.Reader
import Control.Monad.State.Lazy
import qualified Data.Set as Set
import Data.Set (Set, empty, member, singleton, union)
import Prelude hiding (null, tail)

data Symbol = Symbol (Maybe String) String

data Name = Name String
  deriving (Eq, Ord, Show)

data Row = RowEmpty | Row Name Type Row | RVar Name
  deriving (Eq, Ord, Show)

data Type
  = Con Name
  | Var Name
  | Arr Type Type
  | Tap Type Type
  | R   Row
  | Rec Type
  deriving (Eq, Ord, Show)

tpair :: Type -> Type -> Type
tpair t1 t2 = (Tap (Tap (Con (Name "(,)")) t1) t2)

data Scheme = Forall [Name] Type

instance Show Scheme where
  show (Forall as t) =
    concat (map (\(Name a) -> "∀" ++ a ++ ".") as) ++ show t

data Literal
  = Boolean Bool
  | Integer Integer
  | String String

instance Show Literal where
  show (Boolean True)  = "true"
  show (Boolean False) = "false"
  show (Integer i)     = show i
  show (String s)      = show s

litType :: Literal -> Type
litType (Boolean _) = Con $ Name "Boolean"
litType (Integer _) = Con $ Name "Integer"
litType (String  _) = Con $ Name "String"

-- newtype Fix f = Fix (f (Fix f))

-- instance Show (f (Fix f)) => Show (Fix f) where
--     showsPrec n (Fix x) = showsPrec n x

data ExpF a
  = Lit Literal
  | Sym Name
  | App a a
  | Lam Name a
  | Tup a a

instance Show a => Show (ExpF a) where
  show (Lit a)          = show a
  show (Sym (Name a))   = a
  show (App a b)        = "(" ++ show a ++ " " ++ show b ++ ")"
  show (Lam (Name x) e) = "(" ++ x ++ " -> " ++ show e ++ ")"
  show (Tup a b)        = "(" ++ show a ++ ", " ++ show b ++ ")"

type Expr = Fix ExpF

data Def = Def Name Expr

instance Show Def where
  show (Def (Name name) (Fix (Lam (Name x) e))) =
    "(" ++ name ++ " " ++ x ++ " -> " ++ show e ++ ")"
  show (Def (Name name) expr) =
    "(" ++ name ++ " " ++ show expr ++ ")"

data Sig = Sig Name Scheme

instance Show Sig where
  show (Sig (Name name) sigma) =
    "(" ++ name ++ " : " ++ show sigma ++ ")"

instance Functor ExpF where
  fmap _ (Lit a  ) = Lit a
  fmap _ (Sym a  ) = Sym a
  fmap f (App a b) = App (f a) (f b)
  fmap f (Lam x e) = Lam x (f e)
  fmap f (Tup a b) = Tup (f a) (f b)

-- instance Foldable ExpF where
--   foldMap f (Lit a  ) = mempty
--   foldMap f (Sym a  ) = mempty
--   foldMap f (App a b) = f a `mappend` f b
--   foldMap f (Lam x e) = f e

-- instance Traversable ExpF where
--   traverse f (Lit a  ) = pure (Lit a)
--   traverse f (Sym a  ) = pure (Sym a)
--   traverse f (App a b) = liftA2 App (f a) (f b)
--   traverse f (Lam x e) = pure (Lam x) <*> (f e)

class FTV a where
  ftv :: a -> Set Name

instance FTV Type where
  ftv (Con _)         = empty
  ftv (Var a)         = singleton a
  ftv (Arr a b)       = ftv a `union` ftv b
  ftv (Tap a b)       = ftv a `union` ftv b
  ftv (R RowEmpty)    = empty
  ftv (R (Row _ t r)) = ftv t `union` ftv (R r)
  ftv (R (RVar a))    = singleton a
  ftv (Rec row)       = ftv row

type Subst = Map Name Type

null :: Subst
null = Map.empty

class Syntax a where
  subst :: Subst -> a -> a

comp :: Subst -> Subst -> Subst
comp s1 s2 = Map.map (subst s1) s2 `Map.union` s1

instance Syntax Type where
  subst _ (Con a)   = Con a
  subst s (Var a)   = findWithDefault (Var a) a s
  subst s (Arr a b) = Arr (subst s a) (subst s b)
  subst s (Tap a b) = Tap (subst s a) (subst s b)
  subst _ (R RowEmpty) = R RowEmpty
  subst s (R (Row l t r)) = let (R r') = subst s (R r) in
                              R $ Row l (subst s t) r'
  subst s (R (RVar a)) = case findWithDefault (Var a) a s of
                           Var a' -> R (RVar a')
                           found  -> found
  subst s (Rec row) = subst s row


instance Syntax Scheme where
  subst s (Forall as t) = Forall as $ subst s' t
    where s' = foldr Map.delete s as

newtype TypeEnv = TypeEnv (Map Name Scheme)

instance Syntax TypeEnv where
  subst s (TypeEnv m) = TypeEnv $ Map.map (subst s) m

type Constraint = (Type, Type)

data InferState = InferState { count :: Int }

data TypeError
  = InfiniteType Name Type
  | UnificationFailure Type Type
  | RowLabelNotFound Name
  | ResolutionFailure Name
  deriving Show

-- type Infer = RWST TypeEnv [Constraint] InferState (Except TypeError)
type Infer a = ReaderT TypeEnv (StateT Int (Except TypeError)) a

type Unification = Type -> Type -> Infer Subst

occurs :: FTV a => Name -> a -> Bool
occurs a t = member a (ftv t)

bind :: Name -> Type -> Infer Subst
bind a t | t == Var a = return null
         | occurs a t = throwError $ InfiniteType a t
         | otherwise  = return $ Map.singleton a t

rewriteRow :: Name -> Type -> Row -> Infer (Subst, Type)
rewriteRow l _ RowEmpty = throwError $ RowLabelNotFound l
rewriteRow l t (Row l' t' tail) | l == l' = do s <- unify t t'
                                               return (s, R tail)
rewriteRow l t (Row l' t' tail) = do (s, R tail') <- rewriteRow l t tail
                                     return (s, R $ Row l' t' tail')
rewriteRow l t (RVar a) = let row = R $ Row l t (RVar a) in
                            return (Map.singleton a row, row)

unify :: Unification
unify (Arr a b) (Arr a' b') = do
  s1 <- unify a a'
  s2 <- subst s1 b `unify` subst s1 b'
  return $ comp s2 s1
unify (Var a) t = bind a t
unify t (Var a) = bind a t
unify (Con a) (Con b) | a == b = return null
unify (R RowEmpty) (R RowEmpty) = return null
unify (R (Row l t tail)) (R row) = do
  (s1, row') <- rewriteRow l t row
  s2 <- unify (subst s1 (R tail)) row'
  return $ s2 `comp` s1
unify (Rec row) (Rec row') = unify row row'
unify a b = throwError $ UnificationFailure a b


fresh :: Infer Type
fresh = do
  i <- get
  _ <- modify (+1)
  return . Var . Name $ "a" ++ show i

instantiate :: Scheme -> Infer Type
instantiate (Forall as t) = do
  as' <- mapM (const fresh) as
  let s1 = Map.fromList $ zip as as'
  return $ subst s1 t

instance FTV Scheme where
  ftv (Forall as t) = ftv t `Set.difference` Set.fromList as

instance FTV a => FTV [a] where
  ftv = foldr (Set.union . ftv) Set.empty

instance FTV TypeEnv where
  ftv (TypeEnv env) = ftv $ Map.elems env

generalize :: TypeEnv -> Type -> Scheme
generalize env t = Forall as t where
  as = Set.toList $ ftv t `Set.difference` ftv env

resolve :: Name -> Infer (Name, Scheme)
resolve a = do
  (TypeEnv env) <- ask
  case Map.lookup a env of
    Just t  -> return (a, t)
    Nothing -> throwError $ ResolutionFailure a

inEnv :: Name -> Scheme -> Infer a -> Infer a
inEnv x sc m = do
  let scope (TypeEnv e) = TypeEnv $ Map.insert x sc e
  local scope m

data AstF a = Node Subst (ExpF a) Type
type Ast = Fix AstF

infer :: Expr -> Infer Ast

infer (Fix (Lit a)) =
  return $ Fix $ Node null (Lit a) $ litType a

infer (Fix (Sym a)) = do
  (_, s) <- resolve a
  fmap (Fix . Node null (Sym a)) $ instantiate s

infer (Fix (App a b)) = do
  tv <- fresh
  e1@(Fix (Node s1 _ t1)) <- infer a
  e2@(Fix (Node s2 _ t2)) <- local (subst s1) $ infer b
  s3 <- subst s2 t1 `unify` Arr t2 tv
  return $ Fix $ Node (s3 `comp` s2 `comp` s1) (App e1 e2) (subst s3 tv)

infer (Fix (Lam x e)) = do
  tv <- fresh
  e'@(Fix (Node s1 _ t1)) <- inEnv x (Forall [] tv) $ infer e
  return $ Fix $ Node s1 (Lam x e') $ Arr (subst s1 tv) t1

infer (Fix (Tup a b)) = do
  a'@(Fix (Node s1 _ t1)) <- infer a
  b'@(Fix (Node s2 _ t2)) <- local (subst s1) $ infer b
  return $ Fix $ Node (s2 `comp` s1) (Tup a' b') (tpair t1 t2)

runInfer :: TypeEnv -> Expr -> Either TypeError Type
runInfer env expr =
  case runExcept (runStateT (runReaderT (infer expr) env) 0) of
    Right (Fix (Node _ _ t), _) -> Right t
    Left e -> Left e
