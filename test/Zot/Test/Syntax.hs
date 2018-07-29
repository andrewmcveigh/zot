module Zot.Test.Syntax where

import Core
import Syntax
import Zot.Test.Instances()

import Test.Tasty
import Test.Tasty.QuickCheck as QC

tests :: TestTree
tests
  = testGroup
      "Syntax tests"
      [ prop_roundTrip ]

prop_roundTrip :: TestTree
prop_roundTrip
  = testProperty "parse . unparse == Right identity" $
      \e -> parse (unparse e) == Right e
