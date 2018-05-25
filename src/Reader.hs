module Reader where

import Data.Char
import qualified Data.Map.Strict as Map
import Control.Monad
import Control.Applicative
import qualified Prelude as Prelude
import Prelude hiding (read, until)
import Syntax hiding (bind)

newtype Parser a = Parser { parse :: String -> [(a, String)] }

runParser :: Parser a -> String -> a
runParser m s =
  case parse m s of
    [(res, [])] -> res
    [(_,   rs)] -> error "Parser did not consume entire stream."
    _           -> error "Parser error."

item :: Parser Char
item = Parser $ \s ->
  case s of
   []     -> []
   (c:cs) -> [(c, cs)]

bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f = Parser $ \s -> concatMap (\(a, s') -> parse (f a) s') $ parse p s

unit :: a -> Parser a
unit a = Parser (\s -> [(a, s)])

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

combine :: Parser a -> Parser a -> Parser a
combine p q = Parser (\s -> parse p s ++ parse q s)

failure :: Parser a
failure = Parser (\cs -> [])

option :: Parser a -> Parser a -> Parser a
option  p q = Parser $ \s ->
  case parse p s of
    []     -> parse q s
    res    -> res

-- -- | One or more.
-- some :: Alternative f => f a -> f [a]
-- some v = some_v
--   where
--     many_v = some_v <|> pure []
--     some_v = (:) <$> v <*> many_v

-- -- | Zero or more.
-- many :: f a -> f [a]
-- many v = many_v
--   where
--     many_v = some_v <|> pure []
--     some_v = (:) <$> v <*> many_v

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = item `bind` \c ->
  if p c
  then unit c
  else (Parser (\cs -> []))

is :: [Char] -> Parser [Char]
is [] = return []
is (x:xs) = do c <- satisfy (== x); cs <- is xs; return $ c : cs

oneOf :: [Char] -> Parser Char
oneOf s = satisfy (flip elem s)

noneOf :: [Char] -> Parser Char
noneOf s = satisfy (not . (flip elem s))

whitespaceChars = " \n\r\t"
macroChars = ",\";@^`~()[]{}\\"

whitespace :: Parser String
whitespace = many $ oneOf whitespaceChars

macroTerminating :: Parser String
macroTerminating = some $ oneOf macroChars

tokenEnd :: Parser String
tokenEnd = whitespace <|> macroTerminating

token :: Parser [Char]
token = many $ noneOf (whitespaceChars ++ macroChars)

until :: Char -> Parser String
until c = do s <- many $ noneOf [c]; some $ oneOf [c]; return s

string :: Parser Expr
string = do is "\""; e <- until '"'; return $ Fix $ Lit $ Syntax.String e

symbol :: Parser Expr
symbol = do tok <- token; return $ Fix $ Sym $ Name tok

natural :: Parser Expr
natural = do s <- some (satisfy isDigit)
             return $ Fix $ Lit $ Syntax.Integer $ Prelude.read s

delimited :: Char -> Parser Expr
delimited c = do s <- expr; _ <- oneOf [c]; return s

openParen :: Parser Char
openParen = oneOf "("

closeParen :: Parser Char
closeParen = oneOf ")"

parenthesized :: Parser a -> Parser a
parenthesized p = do _ <- openParen; e <- p; _ <- closeParen; return e

openBracket :: Parser Char
openBracket = oneOf "["

closeBracket :: Parser Char
closeBracket = oneOf "]"

bracketed :: Parser a -> Parser a
bracketed p = do _ <- openBracket; e <- p; _ <- closeBracket; return e

lambdaInner :: Parser Expr
lambdaInner = do
  x <- token
  whitespace
  is "->"
  whitespace
  e <- expr
  return $ Fix $ Lam (Name x) e

lambda :: Parser Expr
lambda = parenthesized lambdaInner

application = parenthesized $ do
  e1 <- expr
  whitespace
  e2 <- expr
  return $ Fix $ App e1 e2

tuple = parenthesized $ do
  e1 <- expr
  whitespace
  is ","
  whitespace
  e2 <- expr
  return $ Fix $ Tup e1 e2

expr :: Parser Expr
expr = lambda <|> application <|> tuple <|> string <|> natural <|> symbol

def :: Parser Def
def = do
  openParen
  name <- token
  whitespace
  e <- lambdaInner <|> expr
  closeParen
  return $ Def (Name name) e

toplevel :: Parser Def
toplevel = def

read :: String -> Expr
read s = runParser expr s

tcon :: Parser Type
tcon = do
  c <- oneOf ['A'..'Z']
  cs <- token
  return $ Con $ Name $ c : cs

tvar :: Parser Type
tvar = do
  c <- oneOf ['a'..'z']
  cs <- token
  return $ Var $ Name $ c : cs

arrowInner :: Parser Type
arrowInner =
  do a <- type'; whitespace; is "->"; whitespace; b <- type'; return $ Arr a b

arrow :: Parser Type
arrow = parenthesized arrowInner

tapp :: Parser Type
tapp = parenthesized $ do
  t1 <- type'
  t2 <- type'
  return $ Tap t1 t2

type' :: Parser Type
type' = arrow <|> tapp <|> tvar <|> tcon

readSig :: String -> Type
readSig s = runParser type' s

subPart :: Parser [(Name, Type)]
subPart = do
  (Var a) <- tvar
  whitespace
  is "/"
  whitespace
  t <- type'
  return $ [(a, t)]

moreParts :: Parser [(Name, Type)]
moreParts = parts <|> unit []
  where parts = do whitespace; is ","; whitespace
                   s1 <- subPart
                   ss <- moreParts
                   return $ s1 ++ ss

substitution :: Parser Subst
substitution = bracketed $ do
  s1 <- subPart <|> unit []
  ss <- moreParts
  return $ Map.fromList $ s1 ++ ss

readSub :: String -> Subst
readSub s = runParser substitution s
