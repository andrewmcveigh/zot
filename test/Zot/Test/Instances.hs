{-# OPTIONS_GHC -fno-warn-orphans #-}

module Zot.Test.Instances where

import Core
import Reader.Types as Reader
import Types

import Test.Tasty.QuickCheck

instance Arbitrary Text where
  arbitrary = pack <$> arbitrary

instance Arbitrary Token where
  arbitrary = arbitrary `suchThatMap` mkToken

instance Arbitrary Name where
  arbitrary = arbitrary `suchThatMap` mkName

instance Arbitrary Literal where
  arbitrary = oneof [ pure Unit
                    , Boolean <$> arbitrary
                    , Keyword <$> arbitrary
                    , Integer <$> arbitrary
                    , String  <$> arbitrary
                    ]

instance Arbitrary Types.Lambda where
  arbitrary = Types.Lambda <$> arbitrary <*> arbitrary

instance Arbitrary Reader.Lambda where
  arbitrary = Reader.Lambda <$> arbitrary <*> arbitrary

instance Arbitrary Expr where
  arbitrary = oneof [ Types.Lit <$> arbitrary
                    , Types.Sym <$> arbitrary
                    , Types.App <$> arbitrary <*> arbitrary
                    , Types.Lam <$> arbitrary
                    ]

instance Arbitrary Sexp where
  arbitrary = oneof [ Last <$> arbitrary
                    , Sexp <$> arbitrary <*> arbitrary
                    ]

instance Arbitrary Syntax where
  arbitrary = oneof [ Reader.Lit <$> arbitrary
                    , Reader.Sym <$> arbitrary
                    , Reader.Lam <$> arbitrary
                    , Reader.Sxp <$> arbitrary
                    ]
