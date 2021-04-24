-- Copyright 2019-2021 Google LLC
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

-- | A data type of run-length-encoded lists.
--
-- This module is meant to be imported qualified with the exception of the type
-- RLE itself.  It exports names that clash with things in Prelude and many
-- other data structure modules.

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Data.RLE
         ( RLE, Run(..)
         , toList, toRuns
         , fromList, fromRuns
         , cons, consRun
         , uncons, unconsRun
         , reverse
         , singleton
         , splitAt, take
         , init, null, length, empty
         , (++)
         , map, mapInvertible, traverse, zipWith
         , runs
         ) where

import Prelude hiding
         ( (++), init, length, map, null, reverse
         , splitAt, take, traverse, zipWith
         )
import qualified Prelude as P

import Control.Applicative (Applicative(..))
import Control.Monad (replicateM)
import Data.Coerce (coerce)
import Data.Functor.Contravariant (Contravariant(..))
import Data.Maybe (fromJust)
import Data.Semigroup (Semigroup(stimes))
import Data.Void (absurd)
import GHC.Exts (IsList, IsString(..))
import qualified GHC.Exts (IsList(..))
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)

import Control.DeepSeq (NFData)
import Data.Portray (Portray(..), Portrayal(..))
import Data.Portray.Diff (Diff(..))
import Data.Serialize (Serialize)
import Data.Wrapped (Wrapped(..))

infixr 5 :><
data Run a = Int :>< a
  deriving stock (Eq, Show, Generic, Functor)
  deriving anyclass (NFData, Serialize)
  deriving (Portray, Diff) via Wrapped Generic (Run a)

instance Foldable Run where foldMap f (n :>< x) = stimes n (f x)

-- | After all, why not?
--
-- This is basically Writer (Product Int).
instance Applicative Run where
  pure = (1 :><)
  liftA2 f (m :>< x) (n :>< y) = m*n :>< f x y
  (m :>< f) <*> (n :>< x) = m*n :>< f x

instance Monad Run where (m :>< x) >>= f = case f x of n :>< y-> m*n :>< y

-- Invariant: 'RLE' never contains two adjacent entries with equal @a@ values.
-- Invariant: 'RLE' never contains zero-length runs.
--
-- These two together ensure we can use generated Eq/Ord instances and can
-- implement certain functions faster by omitting tests for zero or duplicated
-- runs.

-- | A run-length encoded representation of a @[a]@.
--
-- This doesn't have a 'Functor' or 'Traversable' instance because it would
-- need an 'Eq' constraint on the element type to uphold invariants, but there
-- are 'map' and 'traverse' functions exported.
newtype RLE a = RLE
  { toRuns :: [Run a]
    -- ^ Extract the contents of an 'RLE' as a list of runs.
    --
    -- This is not a retraction of 'fromRuns': @toRuns . fromRuns@ merges
    -- adjacent runs of equal values and eliminates empty runs.
  }
  deriving stock (Eq, Show, Generic, Foldable)
  deriving anyclass (NFData, Serialize)

instance Portray a => Portray (RLE a) where
  portray rle = Apply "fromRuns" [List $ portray <$> toRuns rle]

instance (Portray a, Diff a) => Diff (RLE a) where
  diff x y = Apply "fromRuns" . pure <$> diff (toRuns x) (toRuns y)

instance Eq a => IsList (RLE a) where
  type Item (RLE a) = a
  fromList = fromList
  toList = toList

instance a ~ Char => IsString (RLE a) where
  fromString = fromList

instance Eq a => Semigroup (RLE a) where
  (<>) = (++)

  stimes 0  _               = empty
  stimes _  (RLE [])        = empty
  stimes n  (RLE [nx :>< x]) = RLE [(fromIntegral n * nx :>< x)]
  stimes n0 (RLE (r0:rs0))  = RLE $ go (n0 - 1) rs0
   where
    adjustedCycle = toRuns $ RLE rs0 ++ RLE [r0]

    go 0 rs = r0:rs
    go n rs = go (n-1) (adjustedCycle P.++ rs)

instance Eq a => Monoid (RLE a) where
  mempty = empty

-- | An empty 'RLE'.
empty :: RLE a
empty = RLE []

-- | Returns 'True' iff the argument contains no elements.
null :: RLE a -> Bool
null = P.null . toRuns

length :: RLE a -> Int
length (RLE rs0) = go rs0
 where
  go [] = 0
  go ((n :>< _) : rs) = n + go rs

fromList :: Eq a => [a] -> RLE a
fromList = foldr cons empty

toList :: RLE a -> [a]
toList (RLE [])          = []
toList (RLE ((n :>< x):xs)) = replicate n x <> toList (RLE xs)

-- | Add an element onto the beginning of the sequence.
cons :: Eq a => a -> RLE a -> RLE a
cons = consRun . (1 :><)

consRun_ :: Eq a => Run a -> [Run a] -> [Run a]
consRun_ (nx :>< x) ((ny :>< y) : rs) | x == y = (nx+ny :>< x) : rs
consRun_ (0 :>< _)  rs                         =                 rs
consRun_ r          rs                         = r             : rs

-- | Add a run of equal elements onto the beginning of the sequence.
consRun :: forall a. Eq a => Run a -> RLE a -> RLE a
consRun = coerce (consRun_ @a)

-- | Split the first element from the rest of the sequence.
uncons :: Eq a => RLE a -> Maybe (a, RLE a)
uncons (unconsRun -> Just (n :>< a, rest)) = Just (a, consRun (n-1 :>< a) rest)
uncons _                                   = Nothing

-- | Split the first run of equal elements from the rest of the sequence.
unconsRun :: RLE a -> Maybe (Run a, RLE a)
unconsRun (RLE (r:rs)) = Just (r, RLE rs)
unconsRun _            = Nothing

take :: Int -> RLE a -> RLE a
take n (RLE xs) = RLE (go n xs)
  where
    go 0 _ = []
    go _ [] = []
    go i ((l :>< x):rs) = (min i l :>< x) : go (max 0 (i - l)) rs

-- | Returns a tuple where the first element contains the first n elements of
-- the sequence, and the second element is the remainder of the sequence.
splitAt :: (HasCallStack, Eq a) => Int -> RLE a -> (RLE a, RLE a)
splitAt n rle = go rle n empty
  where
    go r i prev
      | null r || i <= 0 = (reverse prev, r)
      | i < len = ( reverse ((i :>< a) `consRun` prev)
                  , consRun (len - i :>< a) r')
      | otherwise = go r' (i - len) ((len :>< a) `consRun` prev)
      where
        -- Safe since we check for null above
        ((len :>< a), r') = fromJust $ unconsRun r

-- | Reverse the order of the elements in the sequence.
reverse :: RLE a -> RLE a
reverse (RLE r) = RLE (P.reverse r)

-- | Creates an RLE with a single element.
singleton :: a -> RLE a
singleton a = RLE [1 :>< a]

-- | Append two sequences.
(++) :: Eq a => RLE a -> RLE a -> RLE a
(++) (RLE (x0:xs@(_:_))) = \ys -> RLE $ x0 : toRuns (RLE xs ++ ys)
(++) (RLE [r])           = consRun r
(++) (RLE [])            = id

-- | Map the given function over each element of the sequence.
map :: Eq b => (a -> b) -> RLE a -> RLE b
map f (RLE xs) = fromRuns (fmap (fmap f) xs)

-- | Map the given invertible function over each element of the sequence. This
-- is only safe when the function is invertible.
--
-- This is slightly faster than @map@ and does not require an Eq constraint on
-- the result type.
mapInvertible :: (a -> b) -> RLE a -> RLE b
mapInvertible f (RLE xs) = RLE (fmap (fmap f) xs)

-- | Visit each element of the sequence in an 'Applicative'.
--
-- @
--     traverse :: Eq b => Traversal (RLE a) (RLE b) a b
-- @
traverse :: (Eq b, Applicative f) => (a -> f b) -> RLE a -> f (RLE b)
traverse f rle = case unconsRun rle of
  Nothing           -> pure empty
  Just (n :>< x, rs) -> flip (foldr cons)
    <$> replicateM n (f x)
    <*> traverse f rs

-- | 'Fold' over the contained runs in order.
--
-- This is as strong a type as this can have without breaking any laws, due to
-- the invariants that no empty or mergeable runs exist: if we make it a
-- 'Traversal', it can end up changing the number of targets, and if we make it
-- an 'Iso' to @[(Int, a)]@, the reverse direction is not an isomorphism.
--
-- If you want to use a law-breaking 'Iso' or 'Traversal' for this anyway, use
-- @iso 'fromRuns' 'toRuns'@ to inline the problematic Iso.
--
-- @
--     runs :: Fold (RLE a) (Int, a)
-- @
runs
  :: (Contravariant f, Applicative f)
  => (Run a -> f (Run a))
  -> RLE a -> f (RLE a)
runs f rle = fmap absurd $ contramap absurd $ P.traverse f $ toRuns rle

-- | Construct an 'RLE' from a list of runs.
--
-- This is a retraction of 'toRuns'.
fromRuns :: Eq a => [Run a] -> RLE a
fromRuns = foldr consRun empty

-- | Zip two sequences together.
zipWith :: Eq c => (a -> b -> c) -> RLE a -> RLE b -> RLE c
zipWith f (RLE xs0) (RLE ys0) = RLE $ go xs0 ys0
 where
  go [] _ = []
  go _ [] = []
  go ((nx :>< x) : xs) ((ny :>< y) : ys) = case compare nx ny of
    LT -> (nx :>< f x y) `consRun_` go xs ((ny-nx :>< y) : ys)
    GT -> (ny :>< f x y) `consRun_` go ((nx-ny :>< x) : xs) ys
    EQ -> (nx :>< f x y) `consRun_` go xs ys

init :: HasCallStack => RLE a -> RLE a
init (RLE rs0) = RLE $ go rs0
 where
  go []        = error "RLE.init: empty RLE"
  go (r0:r:rs) = r0 : go (r:rs)
  go [n :>< x] = [n-1 :>< x | n > 1]
