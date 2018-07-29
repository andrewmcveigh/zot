{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Reader where

import Core
import Reader.Types

import Data.Char (isDigit)
-- import qualified Data.Text as Text
import Text.Parsec hiding ((<|>), many, string, token)
-- import qualified Text.Parsec as P
-- import Text.Parsec.Char (spaces)

-- import Debug.Trace

type Parser = Parsec Text ()

is :: [Char] -> Parser [Char]
is = foldr (\x -> (<*>) ((:) <$> satisfy (== x))) (pure [])

not :: Parser a -> Parser ()
not p = try p *> fail "Was p when you said it was `not`" <|> pure ()

whitespace :: Parser [Char]
whitespace = many $ oneOf whitespaceChars

macroTerminating :: Parser [Char]
macroTerminating = some $ oneOf macroChars

tokenEnd :: Parser ()
tokenEnd
  = lookAhead (end *> pure ()) <|> eof
  where
    end = whitespace <|> macroTerminating <|> is ". "

fromMaybe :: [Char] -> Maybe a -> Parser a
fromMaybe _   (Just x) = pure x
fromMaybe err Nothing  = fail err

token :: Parser Token
token =
  pack <$> token' >>= Reader.fromMaybe "Couldn't parse token" . mkToken
  where
    token'
      = some (oneOf tokenChars) -- <* P.optional (Reader.not tokenEnd)

until :: Char -> Parser Text
until c = pack <$> many (satisfy (/= c))

untilP :: Parser [Char] -> Parser a -> Parser [[Char]]
untilP p p' = many p <* p'

string :: Parser Literal
string = between (char '"') (char '"') $ do
  s <- wrap . mconcat <$> many valid
  case Core.read (pack s) of
    Just t  -> pure (String t)
    Nothing -> fail ("Invalid string: " <> s)
  where
    wrap s = "\"" <> s <> "\""
    valid = try doubleEscape <|> notEnd
    notEnd = do
      c <- noneOf "\""
      pure [c]
    doubleEscape = do
      c <- char '\\'
      d <- oneOf "\\\""
      pure [c, d]

unit :: Parser Sexp
unit = is "()" *> pure Unit

symbol :: Parser Syntax
symbol = Sym . fromToken <$> token

keyword :: Parser Literal
keyword = Keyword . fromToken <$> (char ':' *> token)

name :: Parser Name
name = fromToken <$> token

integer :: Parser Literal
integer = do
  txt <- some $ satisfy isNumChar
  case Core.read $ pack txt of
    Just i -> pure . Integer $ i
    Nothing -> fail "Wasn't an integer"
  where
    isNumChar c = isDigit c || c == '-'

bool :: Parser Literal
bool =
      is "True"  *> pure (Boolean True)
  <|> is "False" *> pure (Boolean False)

literal :: Parser Literal
literal =
      try string
  <|> try keyword
  <|> try integer
  <|> bool

lit :: Parser Syntax
lit = Lit <$> literal

delimited :: Char -> Parser Syntax
delimited c = syntax <* oneOf [c]

parenthesized :: Parser a -> Parser a
parenthesized = between (char '(') (char ')')

whitespaced :: Parser a -> Parser a
whitespaced = between spaces spaces

openBracket :: Parser Char
openBracket = oneOf "["

closeBracket :: Parser Char
closeBracket = oneOf "]"

bracketed :: Parser a -> Parser a
bracketed p = do _ <- openBracket; e <- p; _ <- closeBracket; return e

binding :: Parser Binding
binding = char '\\' *> (Binding <$> name) <* char '.'

-- lambda :: Parser Syntax
-- lambda = between (char '(') (char ')') $ do
--   x <- lambdaBinding
--   Lam . Lambda x <$> syntax

sexp :: Parser Syntax
sexp
  = parenthesized $ Sxp . toSexp <$> many syntax
  where
    toSexp []     = Unit
    toSexp [x]    = Last x
    toSexp (x:xs) = Sexp x (toSexp xs)

-- tuple = parenthesized $ do
--   e1 <- expr
--   whitespace
--   is ","
--   whitespace
--   e2 <- expr
--   return $ Fix $ Tup e1 e2

syntax :: Parser Syntax
syntax =
  whitespaced $
      (Bnd <$> try binding)
  <|> try sexp
  <|> try symbol
  <|> try lit

-- def :: Parser Def
-- def = do
--   openParen
--   name <- token
--   whitespace
--   e <- lambdaInner <|> expr
--   closeParen
--   return $ Def (Name name) e

-- toplevel :: Parser Def
-- toplevel = def

runParser'
  :: Stream s Identity t
  => Parsec s () a -> s -> Either ParseError a
runParser' p = runParser p () "Text"

read :: Text -> Either ParseError Syntax
read = runParser' syntax

-- runParser natural "1"

-- tcon :: Parser Type
-- tcon = do
--   c <- oneOf ['A'..'Z']
--   cs <- token
--   return $ Con $ Name $ c : cs

-- tvar :: Parser Type
-- tvar = do
--   c <- oneOf ['a'..'z']
--   cs <- token
--   return $ Var $ Name $ c : cs

-- arrowInner :: Parser Type
-- arrowInner =
--   do a <- type'; whitespace; is "->"; whitespace; b <- type'; return $ Arr a b

-- arrow :: Parser Type
-- arrow = parenthesized arrowInner

-- tapp :: Parser Type
-- tapp = parenthesized $ do
--   t1 <- type'
--   t2 <- type'
--   return $ Tap t1 t2

-- type' :: Parser Type
-- type' = arrow <|> tapp <|> tvar <|> tcon

-- readSig :: String -> Type
-- readSig s = runParser type' s

-- subPart :: Parser [(Name, Type)]
-- subPart = do
--   (Var a) <- tvar
--   whitespace
--   is "/"
--   whitespace
--   t <- type'
--   return $ [(a, t)]

-- moreParts :: Parser [(Name, Type)]
-- moreParts = parts <|> unit []
--   where parts = do whitespace; is ","; whitespace
--                    s1 <- subPart
--                    ss <- moreParts
--                    return $ s1 ++ ss

-- substitution :: Parser Subst
-- substitution = bracketed $ do
--   s1 <- subPart <|> unit []
--   ss <- moreParts
--   return $ Map.fromList $ s1 ++ ss

-- readSub :: String -> Subst
-- readSub s = runParser substitution s
