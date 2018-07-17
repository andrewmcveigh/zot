module Types
  ( Annotate(..)
  , Ast
  , Closure
  , Env(..)
  , Expr
  , Lambda
  , Literal(..)
  , Meta(..)
  , Name(..)
  , Type
  , pattern App
  , pattern Arr
  , pattern Closure
  , pattern Cls
  , pattern Con
  , pattern Lam
  , pattern Lambda
  , pattern Lit
  , pattern Prm
  , pattern Sym
  , pattern Var
  , tcon
  , tvar
  , tarr
  , symbol
  , bool
  , keyword
  , integer
  , string
  , apply
  ) where

import Core
import Data.Map.Lazy (Map)
import Data.Functor.Foldable (Fix(..))

newtype Name = Name Text deriving (Eq, Ord, Show)

data TypeF a
  = ConF Name
  | VarF Name
  | ArrF a a
  deriving Functor

type Type = Fix TypeF

pattern Con :: Name -> Type
pattern Con n = Fix (ConF n)

pattern Var :: Name -> Type
pattern Var n = Fix (VarF n)

pattern Arr :: Type -> Type -> Type
pattern Arr f x = Fix (f `ArrF` x)

tcon :: Name -> Type
tcon n = Fix (ConF n)

tvar :: Name -> Type
tvar n = Fix (VarF n)

tarr :: Type -> Type -> Type
tarr f x = Fix (f `ArrF` x)


data Literal
  = Unit
  | Boolean Bool
  | Keyword Name
  | Integer Core.Integer
  | String  Text
  deriving Show

newtype Env = Env { unEnv :: Map Name Expr }

data LambdaF a
  = LambdaF
    { _x :: Name
    , _e :: a
    } deriving (Functor, Show)

type Lambda = LambdaF Expr

data ClosureF a
  = ClosureF
    { _env :: Env
    , _f   :: a
    } deriving Functor

type Closure = ClosureF Lambda

data ExpF a
  = LitF Literal
  | SymF Name
  | AppF a a
  | LamF (LambdaF a)
  | ClsF (ClosureF a)
  | PrmF (Expr -> Expr)
  deriving Functor

type Expr = Fix ExpF

data Meta = Meta
  { _label :: Text
  , _type  :: Type
  }

data Annotate f = Annotate Meta (f (Annotate f))

type Ast = Annotate ExpF

pattern Lit :: Literal -> Expr
pattern Lit l = Fix (LitF l)

pattern Sym :: Name -> Expr
pattern Sym s  = Fix (SymF s)

pattern App :: Expr -> Expr -> Expr
pattern App a b = Fix (AppF a b)

pattern Lam :: Name -> Expr -> Expr
pattern Lam x e = Fix (LamF (LambdaF x e))

pattern Cls :: Env -> Expr -> Expr
pattern Cls e f = Fix (ClsF (ClosureF e f))

pattern Prm :: (Expr -> Expr) -> Expr
pattern Prm p = Fix (PrmF p)

{-# COMPLETE Lit, Sym, App, Lam, Cls, Prm #-}


pattern Lambda :: Name -> Expr -> Lambda
pattern Lambda x e = LambdaF x e

{-# COMPLETE Lambda #-}

pattern Closure :: Env -> Lambda -> Closure
pattern Closure env f = ClosureF env f

{-# COMPLETE Closure #-}

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

bool :: Bool -> Expr
bool b = Fix (LitF (Boolean b))

keyword :: Text -> Expr
keyword name = Fix (LitF (Keyword (Name name)))

integer :: Core.Integer -> Expr
integer n = Fix (LitF (Integer n))

string :: Text -> Expr
string s = Fix (LitF (String s))

symbol :: Text -> Expr
symbol name = Fix (SymF (Name name))

apply :: Expr -> Expr -> Expr
apply f x = Fix (AppF f x)
