module InferTest where

import Control.Lens
import Control.Monad.Except
import Control.Monad.State.Lazy
import Control.Monad.Reader
import qualified Data.Map.Strict as Map
import Test.HUnit
import Prelude hiding (read)
import Reader
import Syntax

one = read "1"
two = read "2"
plus = read "(x -> (y -> ((+ x) y)))"

int = readSig "Integer"

aTob = readSig "(a -> b)"

var a = readSig a

typeEnv = TypeEnv $ Map.fromList [( Name "+" , Forall [] $ readSig "(Integer -> (Integer -> Integer))")]

unifiesTo :: Type -> Type -> String -> Assertion
unifiesTo a b substr =
  case runExcept (runStateT (runReaderT (unify a b) typeEnv) 0) of
     Left _        -> error "Doesn't unify"
     Right (s2, _) -> let s1 = readSub substr in assertEqual "" s1 s2

test1 = TestCase $ unifiesTo int int "[]"
test2 = TestCase $ unifiesTo (var "a") (var "a") "[]"
test3 = TestCase $ unifiesTo (var "a") (var "x") "[a / x]"
test4 = TestCase $ unifiesTo (var "a") int "[a / Integer]"
test5 = TestCase $ unifiesTo (var "x") aTob "[x / (a -> b)]"

tests = TestList [ TestLabel "test1" test1
                 , TestLabel "test2" test2
                 , TestLabel "test3" test3
                 , TestLabel "test4" test4
                 , TestLabel "test5" test5
                 ]

compTest =
  test
  [
    "simple compose" ~:
    assertEqual "" (readSub "[x / a, y / b]") $ readSub "[x / a]" `comp` readSub "[y / b]"

  , "composed replace" ~:
    assertEqual "" (readSub "[a / b, x / (b -> b)]") $ readSub "[a / b]" `comp` readSub "[x / (a -> b)]"
  ]

typeOfExpr :: String -> Type
typeOfExpr s =
   case runExcept (runStateT (runReaderT (infer $ Reader.read s) typeEnv) 0) of
     Right (Fix (Node _ _ t), _) -> t
     Left _ -> error "No type in here"

inferTest =
  test
  [
    "Integer" ~:
    assertEqual "" int $ typeOfExpr "12"

  , "String" ~:
    assertEqual "" (readSig "String") $ typeOfExpr "\"12\""

  , "a -> Integer" ~:
    assertEqual "" (readSig "(a0 -> Integer)") $ typeOfExpr "(a -> 12)"

  , "a -> a" ~:
    assertEqual "" (readSig "(a0 -> a0)") $ typeOfExpr "(a -> a)"

  , "((a -> a) 123)" ~:
    assertEqual "" (readSig "Integer") $ typeOfExpr "((a -> a) 123)"

  , "+" ~:
    assertEqual "" (readSig "(Integer -> (Integer -> Integer))") $ typeOfExpr "+"

  , "(+ 1)" ~:
    assertEqual "" (readSig "(Integer -> Integer)") $ typeOfExpr "(+ 1)"

  , "((+ 1) 2)" ~:
    assertEqual "" (readSig "Integer") $ typeOfExpr "((+ 1) 2)"

  -- , "(1, 2)" ~:
  --   assertEqual "" (readSig "Integer") $ typeOfExpr "((+ 1) 2)"
  ]

-- >>> Test.HUnit.runTestTT InferTest.inferTest

-- >>> Test.HUnit.runTestTT InferTest.compTest

-- >>> Test.HUnit.runTestTT InferTest.tests
