{-# OPTIONS_GHC -fno-warn-orphans #-}

module Zot.Test.Instances where

import Core
import Test.Tasty.QuickCheck

instance Arbitrary Text where
  arbitrary = pack <$> arbitrary
