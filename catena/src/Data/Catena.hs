-- Copyright 2020-2021 Google LLC
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides a list-like data structure 'Catena' with O(1) concat.
--
-- This is particularly meant to be used for "long-term" accumulations of
-- lists, in place of @DList@, which is well-suited to "short-term"
-- accumulations of lists.  The latter is represented by a "prepender function"
-- which takes the tail of the list and adds the contents of the @DList@ as a
-- prefix.  This behaves nicely in small scopes like as the 'Monoid' of a
-- 'foldMap', where GHC can optimize away the function entirely, but not so
-- nicely as a structure that's kept in memory for any significant time: it
-- can't have "honest" instances for classes like 'NFData', as everything has
-- to be done via conversion to lists.
--
-- So, 'Catena' implements a similar in-memory structure to the tree of
-- closures you would get from using @DList@, but using algebraic data types
-- rather than closures.  It also ultimately stores its elements in
-- 'SmallArray's rather than in lists, to use fewer data constructors.
--
-- For those familiar with GHC's implementation, this is very similar to its
-- @Bag@ type, but the name @Bag@ could be mixed up with a multiset, so we call
-- it something else that's highly unlikely to have a naming conflict.

module Data.Catena
         ( -- * Catena
           Catena
         , empty, fromList, toList
         , singleton
         , concatMap
         , prepend
           -- ** Operators
         , (🔗)
         ) where

import Prelude hiding (concatMap)

import Control.Applicative (Applicative(..))
import Control.Monad (liftM2)
import qualified Data.Foldable as F
import GHC.Exts (IsList)
import qualified GHC.Exts as Exts (IsList(..))
import GHC.Generics (Generic, Generic1)

import Control.DeepSeq (NFData(..))
import Data.Portray (Portray)
import Data.Portray.Diff (Diff)
import Data.Primitive.SmallArray (SmallArray)
import Data.Serialize (Serialize(..))
import Data.Wrapped (Wrapped(..))

-- Standin for SmallArray to add a Serialize instance.
newtype SerArray a = SerArray { unSerArray :: SmallArray a }
  deriving stock (Generic, Traversable)
  deriving newtype
    ( Functor, Applicative, Monad
    , Foldable -- Traversable is incompatible with DerivingVia
    , IsList
    , Semigroup, Monoid
    )
#if MIN_VERSION_primitive(0, 7, 1)
  deriving anyclass NFData
#else
instance NFData a => NFData (SerArray a) where
  rnf (SerArray arr) = F.foldl' (\ () -> rnf) () arr
#endif

instance Serialize a => Serialize (SerArray a) where
  put = put . F.toList . unSerArray
  get = SerArray . Exts.fromList <$> get

-- | A sequence type with O(1) concatenation, cons, and snoc.
data Catena a
  -- Because of the 3-bit space for pointer tags, we get seven constructors to
  -- work with efficiently.  We'd potentially want one for a Catena consisting
  -- of only a packed SmallArray, but then we'd have had 8, and the overhead of
  -- an extra Empty constructor hurts less when it's paired up with a 'PackedL'
  -- or 'PackedR' containing potentially many elements.
  = Empty
  | Singleton !a
  | Cons !a !(Catena a)
  | Snoc !(Catena a) !a
  | PackedL {-# UNPACK #-} !(SerArray a) !(Catena a)
  | PackedR !(Catena a) {-# UNPACK #-} !(SerArray a)
  | Link !(Catena a) !(Catena a)
  -- TODO(awpr): consider adding constructors for:
  -- - nested catenae: Catena (Catena a) -> Catena a (for efficient concatMap)
  deriving stock
    ( Generic, Generic1
    , Functor, Foldable, Traversable
    )
  deriving
    ( Read, Show, Eq, Ord, Portray, Diff
    ) via Wrapped IsList (Catena a)
  deriving anyclass (NFData, Serialize)

instance IsList (Catena a) where
  type Item (Catena a) = a
  toList = F.toList
  fromList = fromList
  fromListN n = PackedR Empty . Exts.fromListN n

instance Semigroup (Catena a) where
  Empty <> y = y
  x <> Empty = x
  Singleton x <> y = Cons x y
  x <> Singleton y = Snoc x y
  PackedL x Empty <> y = PackedL x y
  PackedR Empty x <> y = PackedL x y
  x <> PackedL y Empty = PackedR x y
  x <> PackedR Empty y = PackedR x y
  x <> y = Link x y

instance Monoid (Catena a) where mempty = Empty

-- | '[]'-style instance: cartesian product.
instance Applicative Catena where
  pure = singleton
  liftA2 = liftM2

-- | '[]'-style instance: concatMap.
instance Monad Catena where (>>=) = flip concatMap

-- | An empty 'Catena'.
empty :: Catena a
empty = Empty

-- | A 'Catena' containing a singular element.
singleton :: a -> Catena a
singleton = Singleton

-- | Convert a list to a 'Catena'.
--
-- The result will be packed to a single 'SmallArray'.
fromList :: [a] -> Catena a
fromList = PackedR Empty . Exts.fromList

-- | Convert a 'Catena' back to a list.
toList :: Catena a -> [a]
toList = F.toList

-- | Map each element to a sub-sequence and con'Catena'te the result.
concatMap :: (a -> Catena b) -> Catena a -> Catena b
concatMap f = foldMap f . toList

-- | Convert a 'Catena' to a list-prepender function, like @DList@.
prepend :: Catena a -> [a] -> [a]
prepend = flip (foldr (:))

{-# DEPRECATED (🔗) "Don't actually use this." #-}
-- | Link two 'Catena'e together end-to-end.
(🔗) :: Catena a -> Catena a -> Catena a
(🔗) = Link
