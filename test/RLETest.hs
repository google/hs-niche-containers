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

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad (replicateM)

import Test.Framework (defaultMain)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
         ( (===), Arbitrary(..), Property
         , Gen, choose, forAll, sized, getPositive
         )

import Data.RLE (RLE, fromList, toList)
import qualified Data.RLE as RLE

arbitrarySizedRLE :: Eq a => Gen a -> Int -> Gen (RLE a)
arbitrarySizedRLE genElement n = do
  maxRun <- arbitrary
  resultList <- replicateM n $ do
    element <- genElement
    runLen <- choose (1, getPositive maxRun)
    pure (runLen, element)
  pure (RLE.fromRuns resultList)

rles :: (Eq a, Arbitrary a) => Gen (RLE a)
rles = sized (arbitrarySizedRLE arbitrary)

prop_reverse :: (Eq a, Show a) => RLE a -> Property
prop_reverse rle = RLE.reverse rle === (fromList . reverse . toList) rle

prop_length :: (Eq a, Show a) => RLE a -> Property
prop_length rle = RLE.length rle === (length . toList) rle

prop_fromList_toList :: (Eq a, Show a) => RLE a -> Property
prop_fromList_toList rle = rle === (fromList . toList) rle

prop_toList_fromList :: (Eq a, Show a) => [a] -> Property
prop_toList_fromList xs = xs === (toList . fromList) xs

prop_take :: forall a . (Arbitrary a, Eq a, Show a) => Property
prop_take =
  forAll rles $ \rle ->
  forAll (choose (-1, RLE.length rle + 1)) $ \i ->
  prop_take' @a i rle

prop_take' :: (Eq a, Show a) => Int -> RLE a -> Property
prop_take' i rle =
  take i (toList rle) === toList (RLE.take i rle)

prop_splitAt :: forall a . (Arbitrary a, Eq a, Show a) => Property
prop_splitAt =
  forAll rles $ \rle ->
  forAll (choose (-1, RLE.length rle + 1)) $ \i ->
  prop_splitAt' @a i rle

prop_splitAt' :: (Eq a, Show a) => Int -> RLE a -> Property
prop_splitAt' i rle = (fromList l, fromList r) === RLE.splitAt i rle
  where (l, r) = splitAt i (toList rle)

main :: IO ()
main = defaultMain
  [ testProperty "reverse" (forAll rles $ prop_reverse @Int)
  , testProperty "length" (forAll rles $ prop_length @Int)
  , testProperty "toList . fromList" (prop_toList_fromList @Int)
  , testProperty "fromList . toList" (forAll rles $ prop_fromList_toList @Int)
  , testProperty "splitAt" (prop_splitAt @Int)
  , testProperty "take" (prop_take @Int)
  ]

