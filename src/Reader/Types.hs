module Reader.Types
  ( Lambda(..)
  , Literal(..)
  , Name
  , Parser(..)
  , Sexp(..)
  , Syntax(..)
  , Token
  , failure
  , fromToken
  , macroChars
  , mkName
  , mkToken
  , unName
  , unToken
  , whitespaceChars
  )
  where

import Core

import Data.Set as Set

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
  intersection allowed original == original
  where
    allowed  = fromList tokenChars
    original = fromList (unpack s)

mkToken :: Text -> Maybe Token
mkToken text = if validToken text then Just (Token text) else Nothing

newtype Name = Name { unName :: Text } deriving (Eq, Ord, Show)

fromToken :: Token -> Name
fromToken = Name . unToken

mkName :: Text -> Maybe Name
mkName text = fromToken <$> mkToken text

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

newtype Parser a = Parser { parse :: Text -> [(a, Text)] }

instance Functor Parser where
  fmap f (Parser cs) = Parser (\s -> [(f a, b) | (a, b) <- cs s])

instance Applicative Parser where
  pure = return
  (Parser cs1) <*> (Parser cs2) = Parser (\s -> [(f a, s2) | (f, s1) <- cs1 s, (a, s2) <- cs2 s1])

instance Monad Parser where
  return = unit
  (>>=)  = bind

instance MonadPlus Parser where
  mzero = failure
  mplus = combine

instance Alternative Parser where
  empty = mzero
  (<|>) = option

bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f = Parser $ \s -> concatMap (\(a, s') -> parse (f a) s') $ parse p s

unit :: a -> Parser a
unit a = Parser (\s -> [(a, s)])

combine :: Parser a -> Parser a -> Parser a
combine p q = Parser (\s -> parse p s ++ parse q s)

failure :: Parser a
failure = Parser (const [])

option :: Parser a -> Parser a -> Parser a
option  p q = Parser $ \s ->
  case parse p s of
    []     -> parse q s
    res    -> res
