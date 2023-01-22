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
{-# LANGUAGE DeriveGeneric, FlexibleContexts #-}
module Data.Graph.Invariant.Output
  ( GraphInvariant(..)
  , Permutation(..)
  , RunStats(..)
  ) where

import           Data.Aeson
import           Data.List.NonEmpty             ( NonEmpty(..) )
import           Data.Sequence                  ( Seq(..) )
import           Data.Text
import qualified Data.Vector.Storable          as VS
import           GHC.Generics
import qualified Numeric.LinearAlgebra         as LA

newtype RunStats = RunStats
  { runTimeSeconds :: Maybe Double
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON RunStats where
  toEncoding = genericToEncoding defaultOptions

newtype Permutation = Permutation { cycles :: Seq (VS.Vector Int) }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON Permutation where
  toEncoding = genericToEncoding defaultOptions

data GraphInvariant = GraphInvariant
  { name                  :: Maybe Text
  , invariantVersion      :: Text
  , invariant             :: LA.Z
  , elementInvariants     :: NonEmpty (VS.Vector LA.Z)
  , isomorphismGenerators :: Seq Permutation
  -- ^ 1-based permutations that generate the isomorphism group.
  , runStats              :: Maybe RunStats
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON GraphInvariant where
  toEncoding = genericToEncoding defaultOptions
