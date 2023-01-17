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
{-# LANGUAGE BangPatterns, FlexibleContexts #-}
module Data.Graph.Invariant.Algebra
  ( graphAlgebra
  ) where

import           Data.Array
import           Data.Array.Base                ( unsafeAt )
import qualified Data.IntSet                   as IS
import           Data.Monoid
import qualified Data.Vector                   as V
import qualified Data.Vector.Generic           as VG
import qualified Data.Vector.Storable          as VS

import           Data.Graph.Invariant.Matrix
import           Data.Graph.Invariant.Perfect
import           Data.Graph.Invariant.Types

ifoldMap' :: (VG.Vector v a, Monoid m) => (Int -> a -> m) -> v a -> m
ifoldMap' f = snd . VG.foldl' (\(!i, !a) x -> (i + 1, a <> f i x)) (0, mempty)
{-# INLINE ifoldMap' #-}

fromArrayMap :: (Ix i) => (a -> b) -> Array i a -> V.Vector b
fromArrayMap f a = V.generate (rangeSize $ bounds a) (f . unsafeAt a)

graphAlgebra :: ColoredGraph -> Algebra
graphAlgebra g = Algebra (cgSize g)
                         (return . invariantMatrixF g)
                         (getAll . iso)
 where
  (i0, _) = bounds (cgGraph g)
  es :: V.Vector IS.IntSet
  es = fromArrayMap (IS.fromList . map (subtract i0)) $ cgGraph g
  f'c :: Int -> Int
  f'c = unsafeAt (cgColors g)
  iso :: VS.Vector Int -> All
  iso p = ifoldMap'
    (\j pj ->
      All (f'c j == f'c pj) <> All (es V.! j == IS.map (p VS.!) (es V.! pj))
    )
    p
