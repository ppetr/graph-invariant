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
import           Data.Text                      ( Text )
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
  { cgGraph   :: Graph
  , cgColors  :: Array Vertex Int
  -- ^ Has the same bounds as the `Graph`.
  , cgComment :: Text
  }
  deriving (Eq, Ord, Show, Read)

cgSize :: ColoredGraph -> Int
cgSize ColoredGraph { cgGraph = g } = let (mn, mx) = bounds g in mx - mn + 1
