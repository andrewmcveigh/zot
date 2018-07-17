module Main where

import Core
import Eval
import Reader

import qualified Data.Text.IO as Text

main :: IO ()
main = do
  t <- Text.getLine
  runEval (Reader.read t) emptyEnv
