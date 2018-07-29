{-# LANGUAGE DerivingStrategies #-}

module Check.Types where

import Core
import Syntax.Types (Name(..))
import Reader.Types (Literal(..))

import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Data.Set (Set)

class HasFTV a where
  ftv :: a -> Set Name

class Substitute a where
  sub :: Sub -> a -> a

data Type
  = Con Name
  | Var Name
  | Arr Type Type
  | App Type Type
  deriving (Eq, Ord, Show)

toType :: Literal -> Type
toType (Boolean _) = Con $ Name "Boolean"
toType (Integer _) = Con $ Name "Integer"
toType (Keyword _) = Con $ Name "Keyword"
toType (String  _) = Con $ Name "String"

unit :: Type
unit = Con $ Name "()"

newtype Sub = Sub { unSub :: Map Name Type }

null :: Sub
null = Sub Map.empty

singleton :: Name -> Type -> Sub
singleton n a = Sub $ Map.singleton n a

data Scheme = Forall [Name] Type

newtype Env = Env (Map Name Scheme)

data TypeError
  = InfiniteType Name Type
  | UnificationFailure Type Type
  | RowLabelNotFound Name
  | ResolutionFailure Name
  deriving Show

newtype InferState = InferState { count :: Integer }
  deriving newtype (Eq, Num, Ord, Show)

type Infer = ReaderT Env (StateT InferState (Except TypeError))

type Unification = Type -> Type -> Infer Sub
