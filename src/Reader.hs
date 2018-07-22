{-# OPTIONS_GHC -fno-warn-orphans #-}

module Reader where

import Core
import Reader.Types

import Data.Char (isDigit)
import qualified Data.Text as Text

runParser :: Parser a -> Text -> a
runParser m s =
  case parse m s of
    [(res, null -> True)] -> res
    [(_, s')]             -> panic ("Parser did not consume entire stream." <> s')
    _                     -> panic "Parser error."

item :: Parser Char
item = Parser $ \case
  (null -> True) -> []
  text           -> [(Text.head text, Text.tail text)]

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = item >>= \c ->
  if p c
  then pure c
  else Parser (const [])

is :: [Char] -> Parser [Char]
is = foldr (\x -> (<*>) ((:) <$> satisfy (== x))) (pure [])

oneOf :: [Char] -> Parser Char
oneOf s = satisfy $ flip elem s

noneOf :: [Char] -> Parser Char
noneOf s = satisfy (not . flip elem s)

whitespace :: Parser [Char]
whitespace = many $ oneOf whitespaceChars

macroTerminating :: Parser [Char]
macroTerminating = some $ oneOf macroChars

tokenEnd :: Parser [Char]
tokenEnd = whitespace <|> macroTerminating

fromMaybe :: Maybe a -> Parser a
fromMaybe (Just x) = pure x
fromMaybe Nothing  = failure

token :: Parser Token
token =
  pack <$> many (oneOf tokenChars) >>=
    Reader.fromMaybe . mkToken

until :: Char -> Parser Text
until c = pack <$> many (satisfy (/= c))

string :: Parser Syntax
string = is "\"" *> (Lit . String <$> until '"') <* is "\""

unit :: Parser Syntax
unit = is "()" *> pure (Lit Unit)

symbol :: Parser Syntax
symbol = Sym . fromToken <$> token

name :: Parser Name
name = fromToken <$> token

integer :: Parser Syntax
integer = do
  txt <- some $ satisfy isNumChar
  case Core.read $ pack txt of
    Just i -> pure . Lit . Integer $ i
    Nothing -> failure
  where
    isNumChar c = isDigit c || c == '-'

delimited :: Char -> Parser Syntax
delimited c = syntax <* oneOf [c]

parenthesized :: Parser a -> Parser a
parenthesized p = oneOf "(" *> p <* oneOf ")"

whitespaced :: Parser a -> Parser a
whitespaced p = whitespace *> p <* whitespace

openBracket :: Parser Char
openBracket = oneOf "["

closeBracket :: Parser Char
closeBracket = oneOf "]"

bracketed :: Parser a -> Parser a
bracketed p = do _ <- openBracket; e <- p; _ <- closeBracket; return e

lambdaBinding :: Parser Name
lambdaBinding = is "\\" *> name <* is "."

lambda :: Parser Syntax
lambda = parenthesized $ Lam <$> (Lambda <$> lambdaBinding <*> syntax)

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
      unit
  <|> string
  <|> integer
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

read :: Text -> Syntax
read = runParser syntax

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
