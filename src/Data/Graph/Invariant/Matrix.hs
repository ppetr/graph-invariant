{-# LANGUAGE DataKinds #-}
module Data.Graph.Invariant.Matrix
  ( F
  , invariantBase
  , invariantMatrix
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

-- | The base invariant matrix raised to the power equal to the number of vertices.
invariantMatrix :: ColoredGraph -> LA.Matrix F
invariantMatrix g =
  let (mn, mx) = bounds $ cgGraph g
  -- The exponent is twice the number of vertices: In one step a color
  -- propagates through an edge, and only in the next step it is multiplied by
  -- the vertex's color.
  in  stimesMonoid (2 * (mx - mn + 1)) (invariantBase g)

-- | Computes the invariant of a graph as a vector of 1s multiplied by its
-- 'invariantMatrix`.
invariant :: ColoredGraph -> F
invariant = sum . map LA.sumElements . LA.toRows . invariantMatrix

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
  loop (d : ds) g@(ColoredGraph g' cs)
    | Just i <- findSmallestIndex (LA.toList u) = loop
      ds
      (ColoredGraph g' (cs // [(i + mn, d)]))
    | otherwise = u
   where
    (mn, _) = bounds g'
    u       = inv_f g
  loop [] _ = error "Should never happen"


iterateInvariant' :: ColoredGraph -> F
iterateInvariant' = LA.sumElements . iterateInvariant f
  where f g = LA.konst 1 (cgSize g) LA.<# invariantMatrix g
