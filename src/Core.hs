module Core
  ( module Protolude
  , module Control.Applicative
  , module Control.Lens
  , module Control.Monad
  , module Data.Text
  , module Control.Monad.Except
  , module Control.Monad.Reader
  , module Control.Monad.State.Lazy
  , map
  , read
  , undefined
  )
  where

import Protolude
  ( Bool(..)
  , Char
  , Eq
  , Either(..)
  , Functor
  , Integer
  , IO
  , Maybe(..)
  , Ord
  , Read
  , Show
  , concatMap
  , const
  , elem
  , flip
  , fromMaybe
  , not
  , panic
  , readMaybe
  , (.)
  , ($)
  , (++)
  , (==)
  )
import qualified Protolude as X

import Control.Applicative
import Control.Lens hiding (noneOf)
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Lazy

import Data.Text
  ( Text
  , null
  , pack
  , unpack
  )
-- import qualified Data.Text as Text

map :: Functor f => (a -> b) -> f a -> f b
map = fmap

read :: Read a => Text -> Maybe a
read = X.readMaybe . unpack

undefined :: a
undefined = panic "undefined"
