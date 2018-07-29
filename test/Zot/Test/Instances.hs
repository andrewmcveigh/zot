{-# OPTIONS_GHC -fno-warn-orphans #-}

module Zot.Test.Instances where

import Core
import Check.Types as Check
import Reader.Types as Reader
import Syntax.Types as Syntax
import Types

import Test.Tasty.QuickCheck

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

instance Arbitrary Types.Lambda where
  arbitrary = Types.Lambda <$> arbitrary <*> arbitrary

instance Arbitrary Reader.Binding where
  arbitrary = Reader.Binding <$> arbitrary

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
  arbitrary = oneof [ Con <$> arbitrary
                    , Var <$> arbitrary
                    , Arr <$> arbitrary <*> arbitrary
                    , Check.App <$> arbitrary <*> arbitrary
                    ]
