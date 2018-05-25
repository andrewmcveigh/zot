module ReaderTest where

import Test.HUnit
import Reader


-- >>> Reader.runParser Reader.token "abcd"
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
