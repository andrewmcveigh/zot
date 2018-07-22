module Zot.Test.Reader where

import Core
import Reader
import Reader.Types
import Zot.Test.Instances()

import Test.Tasty
-- import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Data.Text as Text

import Debug.Trace

tests :: TestTree
tests
  = testGroup
      "Reader tests"
      [ prop_is
      , prop_until
      , prop_stringRead
      , prop_symbolRead
      , prop_nameRead
      , prop_lambdaBinding
      , prop_parenthesized
      , prop_whitespaced
      , prop_roundTrip
      ]


-- prop_tokenRead :: TestTree]
-- prop_tokenRead
--   = QC.testProperty "Token read" $
--       \s ->
--         nonTrivial (isJust . tk $ s) $ runParser token s == tk s
--   where
--     tk = Types.mkToken

prop_is :: TestTree
prop_is
  = QC.testProperty "is" $
      \s ->
        runParser (is s) (pack s) == s

prop_until :: TestTree
prop_until
  = QC.testProperty "until" $
      forAll gen $ \s ->
        runParser (until '"' <* is "\"") s == Text.init s
  where
    gen   = end <$> arbitrary `suchThat` \s -> '"' `notElem` unpack s
    end s = s <> "\""

prop_stringRead :: TestTree
prop_stringRead
  = QC.testProperty "String read" $
      forAll gen $
        \s ->
          case runParser string s of
            Lit (String _) -> True
            _              -> False
  where
    gen    = wrap <$> arbitrary `suchThat` \s -> '"' `notElem` unpack s
    wrap s = "\"" <> s <>  "\""

prop_symbolRead :: TestTree
prop_symbolRead
  = QC.testProperty "Symbol read" $
      \s ->
        nonTrivial (isJust . mkToken $ s) $
          case runParser symbol s of
            Sym _ -> True
            _     -> False

prop_nameRead :: TestTree
prop_nameRead
  = QC.testProperty "Name read" $
      \s ->
        nonTrivial (isJust $ fromToken <$> mkToken s) $
          case runParser name s of
            Name _ -> True
            _      -> False

prop_lambdaBinding :: TestTree
prop_lambdaBinding
  = testProperty "Lambda binding read" $
      forAll gen $
        \s ->
          case runParser lambdaBinding s of
            Name _ -> True
            _      -> False
  where
    gen    = wrap . unName <$> arbitrary
    wrap x = "\\" <> x <> "."

prop_parenthesized :: TestTree
prop_parenthesized
  = testProperty "Parenthesized (Name) read" $
      forAll gen $
        \s ->
          case runParser (parenthesized name) s of
            Name _ -> True
            _      -> False
  where
    gen    = wrap . unName <$> arbitrary
    wrap x = "(" <> x <> ")"

prop_whitespaced :: TestTree
prop_whitespaced
  = testProperty "Whitespaced (Name) read" $
      forAll gen $
        \s ->
          case runParser (whitespaced name) s of
            Name _ -> True
            _      -> False
  where
    gen    = wrap . unName <$> arbitrary
    wrap x = " " <> x <> " "

prop_roundTrip :: TestTree
prop_roundTrip
  = QC.testProperty "expr -> print -> read -> expr == expr" $
      \e ->
        traceShow (pr e) $ traceShow e $
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
