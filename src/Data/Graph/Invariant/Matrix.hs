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
{-# LANGUAGE DataKinds, NumericUnderscores #-}
module Data.Graph.Invariant.Matrix
  ( F
  , invariantBase
  , invariantMatrix
  , invariantMatrixF
  , invariant
  , iterateInvariant
  , iterateInvariant'
  ) where

import           Data.Array                     ( (//)
                                                , bounds
                                                )
import           Data.Foldable                  ( toList )
import           Data.List                      ( find
                                                , sort
                                                )
import           Data.Semigroup                 ( stimesMonoid )
import qualified Numeric.LinearAlgebra         as LA

import           Data.Graph.Dimacs.Parse
import           Data.Graph.Invariant.Types

-- An exponent of a random 31-bit prime.
colorHash :: Int -> F
colorHash 0 = 1
colorHash c = 7884612553 ^ c

-- | Constructs a base invariant matrix as a sum of the incidence matrix and a
-- diagonal matrix of colors.
invariantBase :: ColoredGraph -> LA.Matrix F
invariantBase (ColoredGraph g cs _) =
  LA.assoc (n, n) 0
    $  [ ((i, i), colorHash c) | (i, c) <- zip [0 ..] (toList cs) ]
    ++ [ ((i, u - mn), edgeValue) | (i, es) <- zip [0 ..] (toList g), u <- es ]
 where
  (mn, mx) = bounds g
  n        = mx - mn + 1
  -- | A random 31-bit prime. This has to be so that `quotRem` for `Int64` works
  -- correctly.
  edgeValue :: F
  edgeValue = 2_481_945_953

-- | The base invariant matrix raised to the power equal to the number of vertices.
invariantMatrix :: ColoredGraph -> LA.Matrix F
invariantMatrix g =
  -- The exponent is twice the number of vertices: In one step a color
  -- propagates through an edge, and only in the next step it is multiplied by
  -- the vertex's color.
  stimesMonoid (2 * cgSize g) (invariantBase g)

invariantMatrixF :: ColoredGraph -> LA.Vector F -> LA.Vector F
invariantMatrixF g = (LA.<# invariantMatrix g)

-- | Computes the invariant of a graph as a vector of 1s multiplied by its
-- 'invariantMatrix`.
invariant :: ColoredGraph -> F
invariant = sum . map LA.sumElements . LA.toRows . invariantMatrix

-- | Constructs a base invariant matrix of dimension '2n' as a sum of the
-- incidence matrix and a diagonal matrix of colors.
invariantBase2 :: ColoredGraph -> LA.Matrix F
invariantBase2 (ColoredGraph g cs _) =
  LA.assoc (2 * n, 2 * n) 0
    $  [ (x, colorHash c)
       | (i, c) <- zip [0, 2 ..] (toList cs)
       , x      <- [(i, i), (i + 1, i + 1)]
       ]
    ++ [ x
       | (i, es) <- zip [0, 2 ..] (toList g)
       , u       <- es
       , let j = 2 * (u - mn)
       , x <-
         [ ((i, j)        , ev1)
         , ((i, j + 1)    , ev2)
         , ((i + 1, j)    , ev3)
         , ((i + 1, j + 1), ev4)
         ]
       ]
 where
  (mn, mx) = bounds g
  n        = mx - mn + 1
  -- | Random 31-bit primes.
  ev1, ev2, ev3, ev4 :: F
  ev1 = 2_481_945_953
  ev2 = 1_580_784_713
  ev3 = 2_352_696_371
  ev4 = 5_413_961_851

invariantVector2 :: ColoredGraph -> LA.Vector F
invariantVector2 g = LA.fromList (map toScalar (concat bs))
 where
  n  = cgSize g
  bs = LA.toBlocksEvery
    2
    2
    (  LA.fromBlocks [replicate n (LA.ident 2)]
    <> stimesMonoid (2 * n) (invariantBase2 g)
    )
  toScalar :: LA.Matrix F -> F
  toScalar xs22 =
    let x i j = xs22 LA.! i LA.! j
    in  x 0 0 + 2 ^ (x 0 1) + 3 ^ (x 1 0) + 5 ^ (x 1 1)

-- | Returns the index of the smallest element that appears in the list more than once.
findSmallestIndex :: (Ord a) => [a] -> Maybe Int
findSmallestIndex [] = Nothing
findSmallestIndex xs =
  let vs = sort $ zip xs [0 ..]
  in  (\((_, i), _) -> i)
        <$> find (\((x, _), (y, _)) -> x == y) (zip vs (tail vs))

-- | If there are vertices that can't be distinguished, let's assume there
-- exists an automorphism that maps one to another. Let's pick one from the
-- group of more than 1 elements with the smallest color, change its color and
-- run it through the whole procedure.
iterateInvariant
  :: (ColoredGraph -> LA.Vector F) -> ColoredGraph -> LA.Vector F
iterateInvariant inv_f = loop [2 ..]
 where
  loop (d : ds) g@(ColoredGraph g' cs txt)
    | Just i <- findSmallestIndex (LA.toList u) = loop
      ds
      (ColoredGraph g' (cs // [(i + mn, d)]) txt)
    | otherwise = u
   where
    (mn, _) = bounds g'
    u       = inv_f g
  loop [] _ = error "Should never happen"


iterateInvariant' :: ColoredGraph -> F
iterateInvariant' = LA.sumElements . iterateInvariant invariantVector2
  -- where f g = LA.konst 1 (cgSize g) LA.<# invariantMatrix g
