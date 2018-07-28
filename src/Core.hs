module Core
  ( module Protolude
  , module Control.Applicative
  , module Control.Lens
  , module Control.Monad
  -- , module Data.Functor
  , module Data.Monoid
  , module Data.Semigroup
  , module Data.Text
  , module Debug.Trace
  , module Control.Monad.Except
  , module Control.Monad.Reader
  , module Control.Monad.State.Lazy
  , module GHC.Show
  , Print(..)
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
  -- , Show
  , concatMap
  , const
  , elem
  , flip
  , foldl
  , foldr
  , fromMaybe
  , isJust
  , isNothing
  , not
  , notElem
  , panic
  , readMaybe
  -- , show
  , (.)
  , ($)
  , (++)
  , (==)
  , (/=)
  , (||)
  , (&&)
  )
import qualified Protolude as X

import Control.Applicative
import Control.Lens hiding (noneOf)
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Lazy

import Data.Monoid
  ( mappend
  , mconcat
  , mempty
  )

-- import Data.Functor
--   ( ($>)
--   )
import Data.Semigroup
  ( (<>)
  , sconcat
  )

import Data.Text
  ( Text
  , null
  , pack
  , unpack
  , unwords
  , words
  )

import Debug.Trace
  ( trace
  , traceShow
  )

import GHC.Show
  ( Show(..)
  , show
  )
-- import qualified Data.Text as Text

map :: Functor f => (a -> b) -> f a -> f b
map = fmap

read :: Read a => Text -> Maybe a
read = X.readMaybe . unpack

undefined :: a
undefined = panic "undefined"

class Print x where
  pr :: x -> Text
