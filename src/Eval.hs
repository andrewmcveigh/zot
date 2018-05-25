module Eval where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Debug.Trace
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Lazy
import Prelude hiding (lookup, read)
import Reader (read)
import Syntax hiding (bind, subst)

data EvalError
  = ResolveFailure Name
  | CannotApply Expr Expr
  deriving Show

newtype Env = Env (Map Name Expr)

emptyEnv :: Env
emptyEnv = Env Map.empty

emptyTypeEnv :: TypeEnv
emptyTypeEnv = TypeEnv Map.empty

sub :: Name -> Expr -> Expr -> Expr
sub _ (Fix (Lit x  )) _          = Fix $ Lit x
sub s (Fix (Sym x  )) e | s == x = e
sub _ (Fix (Sym x  )) _          = Fix $ Sym x
sub s (Fix (App a b)) e          = Fix $ App (sub s a e) (sub s b e)
sub s (Fix (Lam x a)) _ | s == x = Fix $ Lam x a
sub s (Fix (Lam x a)) e          = Fix $ Lam x (sub s a e)
sub s (Fix (Tup a b)) e          = Fix $ Tup (sub s a e) (sub s b e)

type Eval a = ReaderT Env (StateT Env (Except EvalError)) a

lookup :: Name -> Eval Expr
lookup sym = do
  (Env lexical) <- ask
  (Env global)  <- get
  case Map.lookup sym lexical of
    Just e -> return e
    Nothing -> case Map.lookup sym global of
      Just e -> return e
      Nothing -> throwError $ ResolveFailure sym

bind :: Name -> Expr -> Eval Expr -> Eval Expr
bind sym e m = do
  let scope (Env e') = Env $ Map.insert sym e e' in
    local scope m

expr :: Ast -> Expr
expr (Fix (Node _ (Lit e  ) _)) = Fix $ Lit e
expr (Fix (Node _ (Sym e  ) _)) = Fix $ Sym e
expr (Fix (Node _ (Lam x e) _)) = Fix $ Lam x $ expr e
expr (Fix (Node _ (App a b) _)) = Fix $ App (expr a) (expr b)
expr (Fix (Node _ (Tup a b) _)) = Fix $ Tup (expr a) (expr b)

apply :: Expr -> Expr -> Eval Expr
apply (Fix (Lam x e)) b = return $ sub x e b
apply e b = throwError $ CannotApply e b

eval :: Ast -> Eval Expr
eval (Fix (Node _ (Lit a  ) _)) = return $ Fix $ Lit a
eval (Fix (Node _ (Sym a  ) _)) = lookup a
eval (Fix (Node _ (Lam x e) _)) = return $ Fix $ Lam x $ trace (show $ expr e) $ expr e
eval (Fix (Node _ (App a b) _)) = do
  a' <- eval a
  b' <- eval b
  apply a' b'
eval (Fix (Node _ (Tup a b) _)) = do
  a' <- eval a
  b' <- eval b
  return $ Fix $ Tup a' b'

data REPLError
  = TE TypeError
  | EE EvalError
  deriving Show

runTC :: Expr -> Either TypeError Ast
runTC e =
  fmap fst $ runExcept (runStateT (runReaderT (infer e) emptyTypeEnv) 0)

runREPL :: String -> Either REPLError Expr
runREPL s =
  let e = read s in
    case runTC e of
      Left err -> Left $ TE err
      Right e' ->
        case runExcept (runStateT (runReaderT (eval e') emptyEnv) emptyEnv) of
          Left err -> Left $ EE err
          Right (e'', _) -> Right e''

-- case runTC $ read "(a -> a)" of Right (Fix (Node _ (Lam _ _) _)) -> True

-- >>> runREPL "(((a -> (a -> 2)) 1) 3)"
-- (a -> 2)
-- Right 2
