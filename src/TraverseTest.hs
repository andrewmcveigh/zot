module TraverseTest where

import Test.HUnit
import Syntax

test1 = TestCase (assertEqual
                   ""
                   Syntax.null $
                   unify (Con (Name "Int")) (Con (Name "Int")))

tests = TestList [TestLabel "test1" test1]


-- >>> Test.HUnit.runTestTT TraverseTest.tests
-- Cases: 1  Tried: 0  Errors: 0  Failures: 0                                          Cases: 1  Tried: 1  Errors: 0  Failures: 0
-- Counts {cases = 1, tried = 1, errors = 0, failures = 0}
