module Reader.Types
  ( Binding(..)
  , Literal(..)
  , Name
  , Sexp(..)
  , Syntax(..)
  , Token
  , pattern Name
  , fromList
  , fromToken
  , macroChars
  , mkName
  , mkToken
  , toList
  , tokenChars
  , unName
  , unToken
  , whitespaceChars
  )
  where

import Core
import Syntax.Types hiding (Term(..))

import Data.Set (intersection)
import qualified Data.Set as Set
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
      = ['\''
        -- , '.'
        , '!', '$', '&', '*', '<', '>'
        , '?', '+', '|', '_', ':', '/', '=', '-']
    greek
      = ['λ']

validToken :: Text -> Bool
validToken (Core.null -> True) = False
validToken s =
  intersection allowed original == original &&
  Text.head s `elem` ['a'..'z']
  where
    allowed  = Set.fromList tokenChars
    original = Set.fromList (unpack s)

mkToken :: Text -> Maybe Token
mkToken t = if validToken t then Just (Token t) else Nothing

-- newtype Name = MkName { unName :: Text } deriving (Eq, Ord, Show)
-- pattern Name :: Text -> Name
-- pattern Name t <- MkName t
-- {-# COMPLETE Name #-}

-- instance Print Name where
--   pr (Name t) = t

fromToken :: Token -> Name
fromToken = Name . unToken

mkName :: Text -> Maybe Name
mkName t = fromToken <$> mkToken t

newtype Binding = Binding { unBinding :: Name } deriving (Eq, Show)

instance Print Binding where
  pr (Binding (Name x)) = "\\" <> x <> "."

data Sexp
  = Unit
  | Last Syntax
  | Sexp Syntax Sexp
  deriving (Eq, Show)

fromList :: [Syntax] -> Sexp
fromList []       = Unit
fromList [x]      = Last x
fromList (x : xs) = Sexp x (fromList xs)

toList :: Sexp -> [Syntax]
toList Unit        = []
toList (Last x)    = [x]
toList (Sexp x xs) = x : toList xs

instance Print Sexp where
  pr Unit        = ""
  pr (Last x)    = pr x
  pr (Sexp x xs) = pr x <> " " <> pr xs

data Syntax
  = Lit Literal
  | Sym Name
  | Bnd Binding
  | Sxp Sexp
  deriving (Eq, Show)

instance Print Syntax where
  pr (Lit x)  = pr x
  pr (Sym x)  = unName x
  pr (Bnd x)  = pr x
  pr (Sxp xs) = "(" <> pr xs <> ")"
