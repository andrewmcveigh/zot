module Zot.Test.Reader where

import Core
import Reader
import Reader.Types
import Zot.Test.Instances()

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Data.Text as Text
import Text.Parsec hiding ((<|>), many, string, token)

tests :: TestTree
tests
  = testGroup
      "Reader tests"
      [
      --   prop_is
      -- , prop_until
      -- , prop_stringRead
      -- , prop_symbolRead
      -- , test_tokenRead
      -- , test_trivialLambda
      -- -- , prop_nameRead
      -- , prop_lambdaBinding
      -- , prop_parenthesized
      -- , prop_whitespaced
      -- ,
      -- prop_sexp
      --   test_parenthesized
      -- , test_syntax
      -- , test_sexp
      -- , prop_lambda
        test_string
      --   prop_stringRead
      -- , prop_roundTrip
      -- ,
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
        runParser' (is s) (pack s) == Right s

prop_until :: TestTree
prop_until
  = QC.testProperty "until" $
      forAll gen $ \s ->
        runParser' (until '"' <* is "\"") s == Right (Text.init s)
  where
    gen   = end <$> arbitrary `suchThat` \s -> '"' `notElem` unpack s
    end s = s <> "\""

prop_stringRead :: TestTree
prop_stringRead
  = QC.testProperty "String read" $
      forAll gen $
        \s ->
          case runParser' syntax s of
            Right (Lit (String _)) -> True
            x                      -> traceShow x False
  where
    gen = pr . String <$> arbitrary :: Gen Text

test_string :: TestTree
test_string
  = testCase "Pathological Strings" $
      assertResult "Lit (String \"E\DLE\\\"\")"
        (runParser' syntax $ pr (Lit (String "E\DLE\"")))
        (Lit (String "E\DLE\""))

assertResult
  :: (Show a1, Show a2, Eq a2, Eq a1)
  => [Char] -> Either a2 a1 -> a1 -> Assertion
assertResult s x y
  = assertBool s $
      (x == Right y) || panic (pack (show x))

test_tokenRead :: TestTree
test_tokenRead
  = testCase "Read token" $ do
      assertResult "Parse a token"
        (unToken <$> runParser' token "x") "x"
      assertResult "Parse a token. "
        (unToken <$> runParser' token "x. ") "x"

test_trivialLambda :: TestTree
test_trivialLambda
  = testCase "Read trivial lambda" $ do
      assertResult "Trivial binding \\x."
        (pr <$> runParser' lambdaBinding "\\x.") "x"
      assertResult "Openparen binding (\\x."
        (pr <$> runParser' (char '(' *> lambdaBinding) "(\\x.") "x"
      assertResult "(\\x. e)"
        (pr <$> runParser' lambda "(\\x. e)") "(\\x. e)"
      assertResult "(\\x. e)"
        (pr <$> runParser' syntax "(\\x. e)") "(\\x. e)"

prop_symbolRead :: TestTree
prop_symbolRead
  = QC.testProperty "Symbol read" $
      \s ->
        nonTrivial (isJust . mkToken $ s) $
          case runParser' symbol s of
            Right (Sym _) -> True
            _             -> False

prop_nameRead :: TestTree
prop_nameRead
  = QC.testProperty "Name read" $
      \s ->
        nonTrivial (isJust $ fromToken <$> mkToken s) $
          case runParser' name s of
            Right (Name _) -> True
            _              -> False

prop_lambdaBinding :: TestTree
prop_lambdaBinding
  = testProperty "Lambda binding read" $
      forAll gen $
        \s ->
          case runParser' lambdaBinding s of
            Right (Name _) -> True
            _              -> False
  where
    gen    = wrap . unName <$> arbitrary
    wrap x = "\\" <> x <> ". "

prop_parenthesized :: TestTree
prop_parenthesized
  = testProperty "Parenthesized (Name) read" $
      forAll gen $
        \s ->
          case runParser' (parenthesized name) s of
            Right (Name _) -> True
            _              -> False
  where
    gen    = wrap . unName <$> arbitrary
    wrap x = "(" <> x <> ")"

prop_whitespaced :: TestTree
prop_whitespaced
  = testProperty "Whitespaced (Name) read" $
      forAll gen $
        \s ->
          case runParser' (whitespaced name) s of
            Right (Name _) -> True
            _              -> False
  where
    gen    = wrap . unName <$> arbitrary
    wrap x = " " <> x <> " "

test_syntax :: TestTree
test_syntax
  = testCase "syntax read" $ do
      assertBool "Parse a symbol" $
        isSymbol $ runParser' syntax "x"
      assertBool "Parse a whitespaced symbol" $
        isSymbol $ runParser' syntax " x "
      assertBool "Parse a left whitespaced symbol" $
        isSymbol $ runParser' syntax " x"
      assertBool "Parse a right whitespaced symbol" $
        isSymbol $ runParser' syntax "x "
      assertBool "Parse a symbol'" $
        isSymbol $ runParser' syntax "x'"
      assertBool "Parse a symbol'x" $
        isSymbol $ runParser' syntax "x'BU"
      assertBool "Parse a keyword" $
        isKeyword $ runParser' syntax ":x"
      assertBool "Parse a whitespaced keyword" $
        isKeyword $ runParser' syntax " :x "
      assertBool "Parse a left whitespaced keyword" $
        isKeyword $ runParser' syntax " :x"
      assertBool "Parse a right whitespaced keyword" $
        isKeyword $ runParser' syntax ":x "
      assertBool "Parse the identity lambda" $
        isLambda $ runParser' syntax "(\\x. x)"
  where
    isSymbol  (Right (Sym _))           = True
    isSymbol  _                         = False
    isKeyword (Right (Lit (Keyword _))) = True
    isKeyword x                         = traceShow x False
    isLambda  (Right (Lam _))           = True
    isLambda  _                         = False

test_parenthesized :: TestTree
test_parenthesized =
  testCase "Parse parens" $ do
    assertResult "Parse the trivial sexp"
      (runParser' (parenthesized (char 'x')) "(x)") 'x'
    assertResult "Parse the simple sexp"
      (runParser' (parenthesized (char 'f' *> spaces *> char 'x')) "(f x)") 'x'
    assertResult "Parse the simple sexp with symbols"
      (pr <$> runParser' (parenthesized (symbol *> spaces *> symbol)) "(f x)") "x"
    assertResult "Parse the simple sexp with syntax"
      (pr <$> runParser' (parenthesized (syntax *> spaces *> syntax)) "(f x)") "x"

test_sexp :: TestTree
test_sexp =
  testCase "Parse a simple sexp" $ do
    assertBool "Parse the trivial sexp" $
      isSexp $ runParser' sexp "(f x)"
    assertBool "Parse a simple sexp" $
      isSexp $ runParser' sexp "(h'BU f)"
  where
    isSexp (Right (Sxp _))           = True
    isSexp x                         = traceShow x False

prop_sexp :: TestTree
prop_sexp
  = testProperty "Sexp read" $
      forAll gen $
        \s ->
          case runParser' sexp s of
            Right (Sxp _) -> True
            _             -> False
  where
    gen = do
      f <- arbitrary :: Gen Name
      x <- arbitrary :: Gen Name
      pure ("(" <> unName f <> " " <> unName x <> ")")

prop_lambda :: TestTree
prop_lambda
  = testProperty "Lambda read" $
      forAll gen $
        \s ->
          case runParser' lambda s of
            Right (Lam _) -> True
            _             -> False
  where
    gen = do
      f <- arbitrary :: Gen Name
      x <- arbitrary :: Gen Name
      pure ("(\\" <> unName f <> ". " <> unName x <> ")")

prop_roundTrip :: TestTree
prop_roundTrip
  = QC.testProperty "read . pr == Right identity" $
      \e -> Reader.read (pr e) == Right e

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
