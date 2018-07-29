module Zot.Test.Check where

import Core
import Check
import Check.Types
import Zot.Test.Instances

import Test.Tasty
import Test.Tasty.QuickCheck as QC

tests :: TestTree
tests
  = testGroup
      "Check tests"
      [ prop_infer ]

prop_infer :: TestTree
prop_infer
  = testProperty "parse . unparse == Right identity" $
      forAll wellTyped $
        \(e, t) -> case runInfer env e of
                     Right t' -> t == t'
                     _        -> False
