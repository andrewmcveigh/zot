module Types
  ( Annotate(..)
  -- , Ast
  , Closure
  , Env(..)
  , Expr
  , Lambda
  , Literal(..)
  , Meta(..)
  , Name
  , Primitive(..)
  , Print(..)
  , Token
  , Type
  , pattern App
  -- , pattern Arr
  , pattern Closure
  , pattern Cls
  -- , pattern Con
  , pattern Lam
  , pattern Lambda
  , pattern Lit
  , pattern Prm
  , pattern Sym
  -- , pattern Var
  , tarr
  , tcon
  , tvar
  ) where

import Core
import Reader.Types (Literal(..), Name, Token, unName)

import Data.Functor.Foldable (Fix(..))
import Data.Map.Lazy (Map)

data TypeF a
  = ConF Name
  | VarF Name
  | ArrF a a
  deriving Functor

type Type = Fix TypeF

-- pattern Con :: Name -> Type
-- pattern Con n = Fix (ConF n)

-- pattern Var :: Name -> Type
-- pattern Var n = Fix (VarF n)

-- pattern Arr :: Type -> Type -> Type
-- pattern Arr f x = Fix (f `ArrF` x)

tcon :: Name -> Type
tcon n = Fix (ConF n)

tvar :: Name -> Type
tvar n = Fix (VarF n)

tarr :: Type -> Type -> Type
tarr f x = Fix (f `ArrF` x)


newtype Env = Env { unEnv :: Map Name Expr } deriving (Eq, Show)

data Lambda
  = Lambda
    { _x :: Name
    , _e :: Expr
    } deriving (Eq, Show)

instance Print Lambda where
  pr (Lambda (unName -> x) e) = "(λ" <> x <> ". " <> pr e <> ")"
-- A Lambda is syntactic, but can be destructured at the syntax level

-- type Lambda = LambdaF Expr

data Closure
  = Closure
    { _env :: Env
    , _f   :: Lambda
    } deriving (Eq, Show)

-- type Closure = ClosureF Lambda

newtype Primitive = Primitive (Expr -> Expr)

instance Eq Primitive where
  _ == _ = False
instance Show Primitive where
  show _ = "#<primitive>"

data Expr
  = Lit Literal
  | Sym Name
  | App Expr Expr
  | Lam Lambda -- (LambdaF a)
  | Cls Closure -- (ClosureF a)
  | Prm Primitive
  deriving (Eq, Show)

instance Print Expr where
  pr (Lit x)   = pr x
  pr (Sym x)   = unName x
  pr (App f x) = "(" <> pr f <> " " <> pr x <> ")"
  pr (Lam f)   = pr f
  pr (Cls _)   = "#<closure>"
  pr (Prm _)   = "#<primitive>"

-- type Expr = Fix ExpF

data Meta = Meta
  { _label :: Text
  , _type  :: Type
  }

data Annotate f = Annotate Meta (f (Annotate f))

-- type Ast = Annotate ExpF

-- pattern Lit :: Literal -> Expr
-- pattern Lit l = Fix (LitF l)

-- pattern Sym :: Name -> Expr
-- pattern Sym s  = Fix (SymF s)

-- pattern App :: Expr -> Expr -> Expr
-- pattern App a b = Fix (AppF a b)

-- pattern Lam :: Name -> Expr -> Expr
-- pattern Lam x e = Fix (LamF (LambdaF x e))

-- pattern Cls :: Env -> Expr -> Expr
-- pattern Cls e f = Fix (ClsF (ClosureF e f))

-- pattern Prm :: (Expr -> Expr) -> Expr
-- pattern Prm p = Fix (PrmF (Primitive p))

-- {-# COMPLETE Lit, Sym, App, Lam, Cls, Prm #-}


-- pattern Lambda :: Name -> Expr -> Lambda
-- pattern Lambda x e = LambdaF x e

-- {-# COMPLETE Lambda #-}

-- pattern Closure :: Env -> Lambda -> Closure
-- pattern Closure env f = ClosureF env f

-- {-# COMPLETE Closure #-}

-- class View a where
--   proj :: ExpF a -> a
--   inj  :: a -> ExpF a

-- instance View (ExpF a) where
--   proj = unFix
--   inj  = Fix

-- -- instance View (Annotate a) where
-- --   proj (Annotate _ e) = e
-- --   inj v = Annotated (mkLabel v) v

-- pattern Lit :: Literal -> Expr
-- pattern Lit n <- (proj -> LitF n)
--   where Lit n = inj (LitF n)

-- pattern Sym :: Name -> Expr
-- pattern Sym n <- (proj -> SymF n)
--   where Sym n = inj (SymF n)

-- pattern App :: Expr -> Expr -> Expr
-- pattern App f x <- (proj -> AppF f x)
--   where App f x = inj (AppF f x)

-- pattern Lam :: Name -> Expr -> Expr
-- pattern Lam x e <- (proj -> LamF x e)
--   where Lam x e = inj (LamF x e)
