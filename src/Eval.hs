module Eval where

import Core
import Types

import qualified Data.Map.Strict as Map

data EvalError
  = ResolveFailure Name
  | CannotApply Expr Expr

emptyEnv :: Env
emptyEnv = Env Map.empty

-- emptyTypeEnv :: TypeEnv
-- emptyTypeEnv = TypeEnv Map.empty

type Eval a = StateT Env (Except EvalError) a

lookup :: Name -> Eval Expr
lookup sym = do
  (Env env) <- get
  case Map.lookup sym env of
    Just e  -> pure e
    Nothing -> throwError $ ResolveFailure sym

resolve :: Name -> Eval Expr
resolve a = do
  e <- lookup a >>= eval
  modify (scope e) *> pure e
  where
    scope e (Env env) = Env (Map.insert a e env)

inEnv :: (Env -> Env) -> Eval Expr -> Eval Expr
inEnv f expr = do
  old <- get
  modify f *> expr <* put old

bind :: Name -> Expr -> Eval Expr -> Eval Expr
bind sym e m = inEnv (Env . Map.insert sym e . unEnv) m

close :: Lambda -> Eval Expr
close (Lambda x e) = do
  env <- get
  Cls env . Lam x <$> bind x (Sym x) (close' e)
  where
    close' a@(Lit _  ) = pure a
    close' (Sym a    ) = lookup a
    close' (Lam x' e') = close (Lambda x' e')
    close' (App a  b ) = App <$> close' a <*> close' b
    close' (Cls _ _  ) = panic "Cannot close over a closure"
    close' p@(Prm _  ) = pure p

apply :: Expr -> Expr -> Eval Expr
apply f@(Lam _ _) b         = eval f >>= flip Eval.apply b
apply (Cls env (Lam x e)) b = inEnv (const env) $ bind x b (eval e)
apply e b                   = throwError $ CannotApply e b

eval :: Expr -> Eval Expr
eval a@(Lit _  ) = pure a
eval (Sym a    ) = resolve a
eval (Lam x e  ) = close (Lambda x e)
eval (App a b  ) = Eval.apply a b
eval c@(Cls _ _) = pure c
eval p@(Prm _  ) = pure p

runEval :: Expr -> Env -> Either EvalError Expr
runEval e env =
  case runExcept (runStateT (eval e) env) of
    Left err      -> Left err
    Right (e', _) -> Right e'

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
