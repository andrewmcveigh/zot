module Reader.Types
  ( Lambda(..)
  , Literal(..)
  , Name
  , Sexp(..)
  , Syntax(..)
  , Token
  , pattern Name
  , fromToken
  , macroChars
  , mkName
  , mkToken
  , tokenChars
  , unName
  , unToken
  , whitespaceChars
  )
  where

import Core

import Data.Set as Set
import qualified Data.Text as Text

newtype Token = Token { unToken :: Text }

whitespaceChars :: [Char]
whitespaceChars = " \n\r\t"

macroChars :: [Char]
macroChars = ",\";@^`~()[]{}\\"

tokenChars :: [Char]
tokenChars = ['a'..'z'] <> ['A'..'Z'] <> ['0'..'9'] <> punctuation <> greek
  where
    punctuation
      = ['\'', '.', '!', '$', '&', '*', '<', '>'
        , '?', '+', '|', '_', ':', '/', '=', '-']
    greek
      = ['λ']

validToken :: Text -> Bool
validToken (Core.null -> True) = False
validToken s =
  intersection allowed original == original &&
  Text.head s `elem` ['a'..'z']
  where
    allowed  = fromList tokenChars
    original = fromList (unpack s)

mkToken :: Text -> Maybe Token
mkToken t = if validToken t then Just (Token t) else Nothing

newtype Name = MkName { unName :: Text } deriving (Eq, Ord, Show)
pattern Name :: Text -> Name
pattern Name t <- MkName t
{-# COMPLETE Name #-}

fromToken :: Token -> Name
fromToken = MkName . unToken

mkName :: Text -> Maybe Name
mkName t = fromToken <$> mkToken t

data Literal
  = Unit
  | Boolean Bool
  | Keyword Name
  | Integer Core.Integer
  | String  Text
  deriving (Eq, Show)

instance Print Literal where
  pr Unit               = "()"
  pr (Boolean True)     = "True"
  pr (Boolean False)    = "False"
  pr (Keyword (Name t)) = ":" <> t
  pr (Integer i)        = pack $ show i
  pr (String t)         = "\"" <> t <> "\""

data Lambda
  = Lambda
    { _x :: Name
    , _e :: Syntax
    } deriving (Eq, Show)

instance Print Lambda where
  pr (Lambda (Name x) e) = "(\\" <> x <> ". " <> pr e <> ")"

data Sexp
  = Last Syntax
  | Sexp Syntax Sexp
  deriving (Eq, Show)

instance Print Sexp where
  pr (Last x)    = pr x
  pr (Sexp x xs) = pr x <> " " <> pr xs

data Syntax
  = Lit Literal
  | Sym Name
  | Lam Lambda
  | Sxp Sexp
  deriving (Eq, Show)

instance Print Syntax where
  pr (Lit x)  = pr x
  pr (Sym x)  = unName x
  pr (Lam f)  = pr f
  pr (Sxp xs) = "(" <> pr xs <> ")"
