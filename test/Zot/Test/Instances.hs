{-# OPTIONS_GHC -fno-warn-orphans #-}

module Zot.Test.Instances where

import Core
import Check.Types as Check
import Reader.Types as Reader
import Syntax.Types as Syntax
-- import Types

import Data.Map.Lazy as Map
import Test.Tasty.QuickCheck as QC

instance Arbitrary Text where
  arbitrary = pack <$> arbitrary

instance Arbitrary Token where
  arbitrary = arbitrary `suchThatMap` mkToken

instance Arbitrary Name where
  arbitrary = arbitrary `suchThatMap` mkName

instance Arbitrary Literal where
  arbitrary = oneof [ Boolean <$> arbitrary
                    , Keyword <$> arbitrary
                    , Integer <$> arbitrary
                    , String  <$> arbitrary
                    ]

-- instance Arbitrary Types.Lambda where
--   arbitrary = Types.Lambda <$> arbitrary <*> arbitrary

instance Arbitrary Reader.Binding where
  arbitrary = Reader.Binding <$> arbitrary

-- instance Arbitrary Expr where
--   arbitrary = oneof [ Types.Lit <$> arbitrary
--                     , Types.Sym <$> arbitrary
--                     , Types.App <$> arbitrary <*> arbitrary
--                     , Types.Lam <$> arbitrary
--                     ]

instance Arbitrary Sexp where
  arbitrary = oneof [ Last <$> arbitrary
                    , Sexp <$> arbitrary <*> arbitrary
                    ]

instance Arbitrary Syntax where
  arbitrary = oneof [ Reader.Lit <$> arbitrary
                    , Reader.Sym <$> arbitrary
                    , Reader.Bnd <$> arbitrary
                    , Reader.Sxp <$> arbitrary
                    ]

instance Arbitrary Syntax.Lambda where
  arbitrary = Syntax.Lambda <$> arbitrary <*> arbitrary

instance Arbitrary Term where
  arbitrary = oneof [ pure Syntax.Unit
                    , Syntax.Lit <$> arbitrary
                    , Syntax.Sym <$> arbitrary
                    , Syntax.App <$> arbitrary <*> arbitrary
                    , Syntax.Lam <$> arbitrary
                    ]

instance Arbitrary Check.Type where
  arbitrary = oneof [ boolGen
                    , Var <$> arbitrary
                    , Arr <$> arbitrary <*> arbitrary
                    ]
    where
      boolGen = QC.elements [ Con $ Name "Boolean"
                            , Con $ Name "Integer"
                            , Con $ Name "String"
                            ]

data GEnv = GEnv { _lev :: Integer
                 , _env :: Map Type [Name]
                 }

genv :: GEnv
genv = GEnv 10 Map.empty

bind :: Type -> Name -> GEnv -> GEnv
bind t a (GEnv l m) =
  case Map.lookup t m of
    Just as -> GEnv l $ Map.insert t (a : as) m
    Nothing -> GEnv l $ Map.insert t [a] m

lamAt :: GEnv -> Check.Type -> Check.Type -> Gen Syntax.Lambda
lamAt env t1 t2 = do
  e <- arbitrary
  x <- termAt (bind t1 e env) t2 -- `suchThat` \x -> e `freeIn` x
  pure $ Syntax.Lambda e x
  -- where
  --   _ `freeIn` Syntax.Unit    = False
  --   _ `freeIn` (Syntax.Lit _) = False
  --   a `freeIn` (Syntax.Sym b) = a == b
  --   a `freeIn` (Syntax.App b c) = a `freeIn` b || a `freeIn` c
  --   a `freeIn` (Syntax.Lam (Syntax.Lambda x e)) = x /= a && a `freeIn` e

fromEnv :: GEnv -> Type -> Gen (Maybe Term)
fromEnv (GEnv _ env) t =
  case Map.lookup t env of
    Just xs -> Just . Syntax.Sym <$> QC.elements xs
    Nothing -> pure Nothing

option :: Gen (Maybe a) -> Gen a -> Gen a
option g1 g2 = do
  x1 <- g1
  case x1 of
    Just x -> do
      x2 <- g2
      QC.elements [x, x2]
    Nothing -> g2

appAt :: GEnv -> Type -> Gen Term
appAt env t = do
  t1 <- arbitrary
  Syntax.App <$> (Syntax.Lam <$> lamAt env t1 t) <*> termAt env t

dec :: GEnv -> GEnv
dec env@(GEnv lev _) = env { _lev = lev - 1}

sizeLimit :: GEnv -> Type -> Gen Term -> Gen Term
sizeLimit env@(GEnv l _) t g =
  if l <= 0 then
    fromEnv env t `option`
      case t of
        Con (Name "Boolean") -> Syntax.Lit . Boolean <$> arbitrary
        Con (Name "Integer") -> Syntax.Lit . Integer <$> arbitrary
        Con (Name "String")  -> Syntax.Lit . String  <$> arbitrary
        Con _                -> panic "Impossible pattern match"
        Var _                -> Syntax.Lit . Integer <$> arbitrary
        Arr a b              -> Lam <$> lamAt env a b
  else
    g

termAt
  :: GEnv -> Check.Type -> Gen Term

termAt env t@(Con (Name "Boolean"))
  = sizeLimit env t $
      oneof [ fromEnv env t `option` (Syntax.Lit . Boolean <$> arbitrary)
            , appAt (dec env) (Con (Name "Boolean"))
            ]

termAt env t@(Con (Name "Integer"))
  = sizeLimit env t $
      oneof [ fromEnv env t `option` (Syntax.Lit . Integer <$> arbitrary)
            , appAt (dec env) (Con (Name "Boolean"))
            ]

termAt env t@(Con (Name "String"))
  = sizeLimit env t $
      oneof [ fromEnv env t `option` (Syntax.Lit . String  <$> arbitrary)
            , appAt (dec env) (Con (Name "Boolean"))
            ]

termAt _ (Con (Name x))
  = fail $ "Invalid type constant" <> unpack x

termAt env t@(Var _)
  = sizeLimit env t $
      fromEnv env t `option` notAtVar (dec env)

termAt env t@(Arr t1 t2)
  = sizeLimit env t $
      fromEnv env t `option` (Syntax.Lam <$> lamAt (dec env) t1 t2)

notAtVar :: GEnv -> Gen Term
notAtVar env = do
  t <- arbitrary `suchThat` (not . isVar)
  termAt (dec env) t

isVar :: Type -> Bool
isVar (Var _) = True
isVar _       = False

wellTyped :: Gen (Term, Type)
wellTyped = do
  t <- arbitrary `suchThat` (not . isVar)
  e <- termAt genv t
  pure (e, t)
