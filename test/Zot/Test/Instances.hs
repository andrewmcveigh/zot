{-# OPTIONS_GHC -fno-warn-orphans #-}

module Zot.Test.Instances where

import Core
import Types

import Test.Tasty.QuickCheck

instance Arbitrary Text where
  arbitrary = pack <$> arbitrary

instance Arbitrary Token where
  arbitrary = arbitrary `suchThatMap` mkToken

instance Arbitrary Name where
  arbitrary = Name . unToken <$> arbitrary

instance Arbitrary Literal where
  arbitrary = oneof [ pure Unit
                    , Boolean <$> arbitrary
                    , Keyword <$> arbitrary
                    , Integer <$> arbitrary
                    , String  <$> arbitrary
                    ]

instance Arbitrary Lambda where
  arbitrary = Lambda <$> arbitrary <*> arbitrary

instance Arbitrary Expr where
  arbitrary = oneof [ Lit <$> arbitrary
                    , Sym <$> arbitrary
                    , App <$> arbitrary <*> arbitrary
                    , Lam <$> arbitrary
                    ]
