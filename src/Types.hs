module Types
  ( Annotate(..)
  , Ast
  , Closure(..)
  , Env(..)
  , Expr
  , Lambda
  , Literal(..)
  , Meta(..)
  , Name(..)
  , Thunk(..)
  , Type
  , pattern App
  , pattern Arr
  , pattern Clo
  , pattern Con
  , pattern Lam
  , pattern Lambda
  , pattern Lit
  , pattern Prm
  , pattern Thk
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
  , lambda
  ) where

import Core
import Data.Map.Lazy (Map)
import Data.Functor.Classes (Show1(..))
import Data.Functor.Foldable (Fix(..))
-- import Text.Show.Deriving

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

newtype Env = Env { unEnv :: (Map Name Expr) } deriving Show

data Closure a
  = Closure
    { _env :: Env
    , _f   :: (LambdaF a)
    } deriving (Functor, Show)

newtype Thunk a = Thunk { _closure :: Closure a } deriving (Functor, Show)

data LambdaF a
  = LambdaF
    { _x :: Name
    , _e :: a
    } deriving (Functor, Show)

type Lambda = LambdaF Expr

data Expression a
  = Redex a
  | Normal a

data ExpF a
  = LitF Literal
  | SymF Name
  | AppF a a
  | LamF (LambdaF a)
  | CloF (Closure a)
  | PrmF Name (LambdaF a)
  | ThkF (Thunk a)
  deriving (Functor, Show)

instance Show1 ExpF where
  liftShowsPrec = undefined

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

pattern Clo :: Env -> LambdaF Expr -> Expr
pattern Clo e f = Fix (CloF (Closure e f))

pattern Prm :: Name -> Name -> Expr -> Expr
pattern Prm n x e = Fix (PrmF n (LambdaF x e))

pattern Thk :: Thunk Expr -> Expr
pattern Thk t = Fix (ThkF t)

pattern Lambda :: Name -> Expr -> LambdaF Expr
pattern Lambda x e = LambdaF x e

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

lambda :: Name -> Expr -> Expr
lambda x e = Lam x e


-- mkLabel :: HuttonF Annotated -> Label

-- deriveShow1 ''ExpF
