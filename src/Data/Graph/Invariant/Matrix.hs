{-# LANGUAGE DataKinds #-}
module Data.Graph.Invariant.Matrix
  ( F
  , invariantBase
  , invariantMatrix
  , invariant
  ) where

import           Data.Array                     ( bounds )
import           Data.Foldable                  ( toList )
import           Data.Semigroup                 ( stimesMonoid )
import qualified Numeric.LinearAlgebra         as LA

import           Data.Graph.Dimacs.Parse

-- 2^31-1 https://primes.utm.edu/lists/2small/0bit.html
type F = LA.Mod 0x7fffffff LA.Z

-- A random 31-bit prime. This has to be so that `quotRem` for `Int64` works
-- correctly.
edgeValue :: F
edgeValue = 2481945953

-- An exponent of a random 31-bit prime.
colorHash :: Int -> F
colorHash 0 = 1
colorHash c = 7884612553 ^ c

-- | Constructs a base invariant matrix as a sum of the incidence matrix and a
-- diagonal matrix of colors.
invariantBase :: ColoredGraph -> LA.Matrix F
invariantBase (ColoredGraph g cs) =
  LA.assoc (n, n) 0
    $  [ ((i, i), colorHash c) | (i, c) <- zip [0 ..] (toList cs) ]
    ++ [ ((i, u - mn), edgeValue) | (i, es) <- zip [0 ..] (toList g), u <- es ]
 where
  (mn, mx) = bounds g
  n        = mx - mn + 1

-- | The base invariant matrix raied to the power equal to the number of vertices.
invariantMatrix :: ColoredGraph -> LA.Matrix F
invariantMatrix g = stimesMonoid
  (let (mn, mx) = bounds $ cgGraph g in mx - mn + 1)
  (invariantBase g)

-- | Computes the invariant of a graph as a vector of 1s multiplied by its
-- 'invariantMatrix`.
invariant :: ColoredGraph -> F
invariant = sum . map LA.sumElements . LA.toRows . invariantMatrix
