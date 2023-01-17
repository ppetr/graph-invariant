-- Copyright 2023 Google LLC
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
{-# LANGUAGE FlexibleInstances #-}
module Data.Graph.Invariant.Util
  ( hashNub
  , hashNubBy
  ) where

import           Control.Monad.ST
import           Data.Foldable                  ( forM_ )
import qualified Data.HashTable.ST.Basic       as HT
import           Data.Hashable                  ( Hashable(..) )

-- * Hashable

-- | Returns unique elements of a collection in arbitrary order,
hashNubBy :: (Foldable t, Eq b, Hashable b) => (a -> b) -> t a -> [a]
hashNubBy f xs = runST $ do
  ht <- HT.new
  forM_ xs $ \x -> HT.insert ht (f x) x
  HT.foldM (\ys (_, y) -> return (y : ys)) [] ht
{-# INLINE hashNubBy #-}

-- | Returns unique elements of a collection in arbitrary order,
hashNub :: (Foldable t, Eq a, Hashable a) => t a -> [a]
hashNub = hashNubBy id
{-# INLINE hashNub #-}
