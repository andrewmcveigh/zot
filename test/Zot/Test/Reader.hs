module Zot.Test.Reader where

import Core
import Reader
import Types
import Zot.Test.Instances()

import Test.Tasty
-- import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

tests :: TestTree
tests
  = testGroup
      "Reader tests"
      [ prop_symbolRead
      , prop_roundTrip
      ]

-- prop_tokenRead :: TestTree
-- prop_tokenRead
--   = QC.testProperty "Token read" $
--       \s ->
--         nonTrivial (isJust . tk $ s) $ runParser token s == tk s
--   where
--     tk = Types.mkToken

prop_symbolRead :: TestTree
prop_symbolRead
  = QC.testProperty "Symbol read" $
      \s ->
        nonTrivial (isJust . Types.mkToken $ s) $
          case runParser Reader.symbol s of
            Sym _ -> True
            _     -> False

prop_roundTrip :: TestTree
prop_roundTrip
  = QC.testProperty "expr -> print -> read -> expr == expr" $
      \e ->
        Reader.read (pr e) == e

ignore :: Bool
ignore = True

nonTrivial :: Bool -> Bool -> QC.Property
nonTrivial cond test =
  QC.classify cond "non-trivial" $ if cond then test else ignore

-- >>>
-- "abcd"

-- >>> Reader.runParser Reader.expr "((a -> a) 1)"
-- ((a -> a) 1)

-- >>> Reader.runParser Reader.expr "((a -> a) \"1\")"
-- ((a -> a) "1")

-- >>> Reader.runParser Reader.expr "\"abc\""
-- "abc"

-- >>> Reader.runParser Reader.expr "123"
-- 123

-- >>> Reader.runParser Reader.expr "true"
-- true

-- >>> Reader.runParser Reader.expr "(a, b)"
-- (a, b)

-- >>> Reader.runParser Reader.topLevel "(map f -> (xs -> (f xs)))"
-- (map f -> (xs -> (f xs)))


-- (map : (a -> b) -> [a] -> [b])
-- (map [f xs] ...)
