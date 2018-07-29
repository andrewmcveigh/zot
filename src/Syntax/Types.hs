module Syntax.Types where

import Core

newtype Name = Name { unName :: Text } deriving (Eq, Ord, Show)

instance Print Name where
  pr (Name t) = t

data Literal
  = Boolean Bool
  | Keyword Name
  | Integer Core.Integer
  | String  Text
  deriving (Eq, Show)

instance Print Literal where
  pr (Boolean True)     = "True"
  pr (Boolean False)    = "False"
  pr (Keyword (Name t)) = ":" <> t
  pr (Integer i)        = pack $ show i
  pr (String t)         = pack $ show t

data Lambda
  = Lambda
    { _x :: Name
    , _e :: Term
    } deriving (Eq, Show)

instance Print Lambda where
  pr (Lambda x e) = "(\\" <> pr x <> ". " <> pr e <> ")"

data Term
  = Unit
  | Lit Literal
  | Sym Name
  | App Term Term
  | Lam Lambda
  deriving (Eq, Show)

instance Print Term where
  pr Unit      = "()"
  pr (Lit x)   = pr x
  pr (Sym x)   = pr x
  pr (App x y) = "(" <> pr x <> " " <> pr y <> ")"
  pr (Lam f)   = pr f
