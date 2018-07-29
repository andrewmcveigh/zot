module Syntax where

import Core
import Data.List (foldl')
import qualified Reader.Types as R
import Syntax.Types

parse
  :: R.Syntax -> Either Text Term
parse (R.Lit x)
  = Right $ Lit x
parse (R.Sym x)
  = Right $ Sym $ Name $ R.unName x
parse (R.Bnd _)
  = Left "Illegal syntax: solo binding"
parse (R.Sxp R.Unit)
  = Right Unit
parse (R.Sxp (R.Sexp (R.Bnd (R.Binding (R.Name x))) (R.Last e)))
  = Lam . Lambda (Name x) <$> parse e
parse (R.Sxp (R.Last x))
  = parse x
parse (R.Sxp (R.Sexp x R.Unit))
  = App <$> parse x <*> pure Unit
parse (R.Sxp (R.Sexp x (R.Last y)))
  = App <$> parse x <*> parse y
parse (R.Sxp sexp)
  = case R.toList sexp of
      []     -> panic "Impossible pattern"
      x : xs -> foldl' f (parse x) xs
  where
    f x y = App <$> x <*> parse y

unparse
  :: Term -> R.Syntax
unparse Unit
  = R.Sxp R.Unit
unparse (Lit x)
  = R.Lit x
unparse (Sym (Name x))
  = case R.mkName x of
      Just n  -> R.Sym n
      Nothing -> panic "Like wut!?"
unparse t@(App _ _)
  = let xs = toList t in
      R.Sxp $ R.fromList xs
  where
    toList (App x y) = toList x <> [unparse y]
    toList t'        = [unparse t']
unparse (Lam (Lambda x e))
  = R.Sxp (R.Sexp (R.Bnd x) (R.Last (unparse e)))
