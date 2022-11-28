{-# LANGUAGE DataKinds #-}
module Data.Graph.Invariant.Matrix
  ( F
  , incidence
  , incidence'
  , invariantBase
  , invariantMatrix
  , invariant
  ) where

import           Control.Monad                  ( forM_ )
import           Data.Array                     ( bounds )
import           Data.Foldable                  ( length
                                                , toList
                                                )
import           Data.Graph
import qualified Data.HashMap.Strict           as HM
import           Data.Semigroup                 ( stimesMonoid )
import qualified Data.Vector.Generic           as V
import qualified Data.Vector.Generic.Mutable   as VM
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

-- | Converts a list of neighbours to a vector that maps indices to one of two
-- values depending on the presence of an edge.
incidenceRow
  :: (LA.Element a)
  => (Vertex, Vertex)  -- ^ Bounds of vertices (inclusive).
  -> a  -- ^ Value for no edge.
  -> a  -- ^ Value for an edge.
  -> [Vertex]  -- ^ List of neighbours.
  -> LA.Vector a
incidenceRow (mn, mx) e0 e1 cs = V.create $ do
  vs <- VM.replicate (mx - mn + 1) e0
  forM_ cs $ \c -> VM.write vs (c - mn) e1
  return vs

-- | Given values for "no edge" and "edge" constructs an incidence matrix of a
-- graph.
incidence
  :: (LA.Element a)
  => a  -- ^ Value for no edge.
  -> a  -- ^ Value for an edge.
  -> Graph
  -> LA.Matrix a
incidence e0 e1 g = LA.fromRows (map (incidenceRow bs e0 e1) $ toList g)
  where bs = bounds g

-- | Constructs an incidence matrix of a graph using 0 for "no edge" and an
-- unspecified constant for "edge".
incidence' :: Graph -> LA.Matrix F
incidence' = incidence 0 edgeValue

-- | Extracts colors to a vector.
colors :: ColoredGraph -> LA.Vector F
colors (ColoredGraph g cs) = V.create $ do
  let (mn, _) = bounds g
  vs <- VM.replicate (length g) (colorHash 0)
  forM_ (HM.toList cs) $ \(i, c) -> VM.write vs (i - mn) (colorHash c)
  return vs

-- | Constructs a base invariant matrix as a sum of the incidence matrix and a
-- diagonal matrix of colors.
invariantBase :: ColoredGraph -> LA.Matrix F
invariantBase g = incidence' (cgGraph g) + LA.diag (colors g)

-- | The base invariant matrix raied to the power equal to the number of vertices.
invariantMatrix :: ColoredGraph -> LA.Matrix F
invariantMatrix g = stimesMonoid (length $ cgGraph g) (invariantBase g)

-- | Computes the invariant of a graph as a vector of 1s multiplied by its
-- 'invariantMatrix`.
invariant :: ColoredGraph -> F
invariant = sum . map LA.sumElements . LA.toRows . invariantMatrix
