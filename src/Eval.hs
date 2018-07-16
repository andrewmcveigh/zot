module Eval where

import Core
import qualified Data.Map.Strict as Map
-- import Debug.Trace
-- import Reader (read)
import Types

data EvalError
  = ResolveFailure Name
  | CannotApply Expr Expr


emptyEnv :: Env
emptyEnv = Env Map.empty

-- emptyTypeEnv :: TypeEnv
-- emptyTypeEnv = TypeEnv Map.empty

type Eval a = ReaderT Env (StateT Env (Except EvalError)) a

lookup :: Name -> Eval Expr
lookup sym = do
  (Env lexical) <- ask
  (Env global)  <- get
  case Map.lookup sym lexical of
    Just e -> pure e
    Nothing -> case Map.lookup sym global of
      Just e -> pure e
      Nothing -> throwError $ ResolveFailure sym

bind :: Name -> Expr -> Eval Expr -> Eval Expr
bind sym e m = do
  let scope (Env e') = Env $ Map.insert sym e e' in
    local scope m

apply :: Expr -> Expr -> Eval Expr
apply (Lam x e) b = bind x b (eval e)
apply e b = throwError $ CannotApply e b

close :: Lambda -> Eval Expr
close (Lambda x e) = do
  bind x (Sym x) (close' e)
  where
    close' a@(Lit _  ) = pure a
    close' (Sym a    ) = lookup a
    close' (Lam x' e') = close (Lambda x' e')
    close' (App a  b ) = do a' <- close' a; b' <- close' b; pure (App a' b')
    close' _
      = panic "Impossible pattern match"
close _
  = panic "Impossible pattern match"

thunk :: Expr -> Eval Expr
thunk e = do
  Env lexical <- ask
  Env global  <- get
  let f = (Lambda (Name "_") e)
  pure $ Thk $ Thunk $ Closure (Env (Map.union lexical global)) f

eval :: Expr -> Eval Expr
eval a@(Lit _) = pure a
eval (Sym a  ) = lookup a
eval (Lam x e) = close (Lambda x e)
eval (App a b) = do a' <- eval a; b' <- eval b; Eval.apply a' b'
eval _
  = panic "Impossible pattern match"

-- data REPLError
--   = TE TypeError
--   | EE EvalError
--   deriving Show

-- runTC :: Expr -> Either TypeError Ast
-- runTC e =
--   fmap fst $ runExcept (runStateT (runReaderT (infer e) emptyTypeEnv) 0)

-- runREPL :: String -> Either REPLError Expr
-- runREPL s =
--   let e = read s in
--     case runTC e of
--       Left err -> Left $ TE err
--       Right e' ->
--         case runExcept (runStateT (runReaderT (eval e') emptyEnv) emptyEnv) of
--           Left err -> Left $ EE err
--           Right (e'', _) -> Right e''

-- case runTC $ read "(a -> a)" of Right (Fix (Node _ (Lam _ _) _)) -> True

-- >>> runREPL "(((a -> (a -> 2)) 1) 3)"
-- (a -> 2)
-- Right 2
