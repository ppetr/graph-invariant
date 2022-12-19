{-# LANGUAGE DataKinds, ScopedTypeVariables, TypeFamilies #-}
module Data.Graph.Invariant.Types
  ( F
  , maxBoundF
  , ColoredGraph(..)
  , cgSize
  ) where

import           Data.Array
import           Data.Graph
import           Data.Proxy
import           GHC.TypeLits
import qualified Numeric.LinearAlgebra         as LA

-- 2^31-1 https://primes.utm.edu/lists/2small/0bit.html
type F = LA.Mod 0x7fffffff LA.Z

maxBoundF :: LA.Z
maxBoundF = x - 1
 where
  x :: forall m . (LA.Mod m LA.Z ~ F) => LA.Z
  x = fromIntegral $ natVal (Proxy :: Proxy m)

data ColoredGraph = ColoredGraph
  { cgGraph  :: Graph
  , cgColors :: Array Vertex Int
  -- ^ Has the same bounds as the `Graph`.
  }
  deriving (Eq, Ord, Show, Read)

cgSize :: ColoredGraph -> Int
cgSize (ColoredGraph g _) = let (mn, mx) = bounds g in mx - mn + 1
