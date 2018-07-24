{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Reader where

import Core
import Reader.Types

import Data.Char (isDigit)
-- import qualified Data.Text as Text
import Text.Parsec hiding ((<|>), many, string, token)

import Debug.Trace

type Parser = Parsec Text ()

is :: [Char] -> Parser [Char]
is = foldr (\x -> (<*>) ((:) <$> satisfy (== x))) (pure [])

noneOf :: [Char] -> Parser Char
noneOf s = satisfy (not . flip elem s)

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
  pack <$> manyTill (oneOf tokenChars) tokenEnd >>=
    Reader.fromMaybe "Couldn't parse token" . mkToken

until :: Char -> Parser Text
until c = pack <$> many (satisfy (/= c))

untilP :: Parser [Char] -> Parser a -> Parser [[Char]]
untilP p p' = many p <* p'

string :: Parser Literal
string = is "\"" *> (String <$> until '"') <* is "\""

unit :: Parser Literal
unit = is "()" *> pure Unit

symbol :: Parser Syntax
symbol = Sym . fromToken <$> token

keyword :: Parser Literal
keyword = Keyword . fromToken <$> (is ":" *> token)

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
      is "true"  *> pure (Boolean True)
  <|> is "false" *> pure (Boolean False)

literal :: Parser Literal
literal =
      unit
  <|> string
  <|> keyword
  <|> integer
  <|> bool

lit :: Parser Syntax
lit = Lit <$> literal

delimited :: Char -> Parser Syntax
delimited c = syntax <* oneOf [c]

parenthesized :: Parser a -> Parser a
parenthesized p = is "(" *> p <* is ")"

whitespaced :: Parser a -> Parser a
whitespaced p = whitespace *> p <* whitespace

openBracket :: Parser Char
openBracket = oneOf "["

closeBracket :: Parser Char
closeBracket = oneOf "]"

bracketed :: Parser a -> Parser a
bracketed p = do _ <- openBracket; e <- p; _ <- closeBracket; return e

lambdaBinding :: Parser Name
lambdaBinding = do
  _ <- is "\\"
  n <- trace "HHH" name
  _ <- traceShow ("HHHH" <> unName n) (is "." *> whitespace)
  pure n

lambda :: Parser Syntax
lambda = do
  _ <- is "("
  x <- trace "Here!" lambdaBinding
  _ <- trace "Here2!" $ is ")"
  Lam . Lambda x <$> traceShow x syntax

sexp :: Parser Syntax
sexp
  = parenthesized $ Sxp . toSexp <$> some syntax
  where
    toSexp []     = panic "Impossible"
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
      lit
  <|> symbol
  <|> lambda
  <|> sexp

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
