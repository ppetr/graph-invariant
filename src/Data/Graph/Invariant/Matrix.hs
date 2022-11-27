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
import           Data.Semigroup                 ( stimes )
import qualified Data.Vector.Generic           as V
import qualified Data.Vector.Generic.Mutable   as VM
import qualified Numeric.LinearAlgebra         as LA

import           Data.Graph.Dimacs.Parse

-- https://primes.utm.edu/lists/2small/0bit.html
type F = LA.Mod 9223372036854775783 LA.Z

-- A random 63-bit prime.
edgeValue :: F
edgeValue = 6876016341580807721

-- An exponent of a random 63-bit prime.
colorHash :: Int -> F
colorHash 0 = 1
colorHash c = 3164292571999483261 ^ c

incidenceRow
  :: (LA.Element a) => (Vertex, Vertex) -> a -> a -> [Vertex] -> LA.Vector a
incidenceRow (mn, mx) e0 e1 cs = V.create $ do
  vs <- VM.replicate (mx - mn + 1) e0
  forM_ cs $ \c -> VM.write vs (c - mn) e1
  return vs

incidence :: (LA.Element a) => a -> a -> Graph -> LA.Matrix a
incidence e0 e1 g = LA.fromRows (map (incidenceRow bs e0 e1) $ toList g)
  where bs = bounds g

incidence' :: Graph -> LA.Matrix F
incidence' = incidence 0 edgeValue

colors :: ColoredGraph -> LA.Vector F
colors (ColoredGraph g cs) = V.create $ do
  let (mn, _) = bounds g
  vs <- VM.replicate (length g) (colorHash 0)
  forM_ (HM.toList cs) $ \(i, c) -> VM.write vs (i - mn) (colorHash c)
  return vs

invariantBase :: ColoredGraph -> LA.Matrix F
invariantBase g = incidence' (cgGraph g) + LA.diag (colors g)

invariantMatrix :: ColoredGraph -> LA.Matrix F
invariantMatrix g = stimes n (invariantBase g)
 where
  n =
    ceiling . logBase (2 :: Double) . fromIntegral . length . cgGraph $ g :: Integer

invariant :: ColoredGraph -> F
invariant = sum . map LA.sumElements . LA.toRows . invariantMatrix
