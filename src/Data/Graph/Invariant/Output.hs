{-# LANGUAGE DeriveGeneric #-}
module Data.Graph.Invariant.Output
  ( Orbit(..)
  , GraphInvariant(..)
  ) where

import           Data.Aeson
import           Data.Text
import qualified Data.Vector                   as V
import qualified Data.Vector.Storable          as VS
import           GHC.Generics
import qualified Numeric.LinearAlgebra         as LA

data Orbit = Orbit
  { order      :: Int
  , generators :: V.Vector (V.Vector (LA.Z, LA.Z))
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON Orbit where
  toEncoding = genericToEncoding defaultOptions

data GraphInvariant = GraphInvariant
  { name              :: Text
  , invariantVersion  :: Text
  , invariant         :: LA.Z
  , elementInvariants :: VS.Vector LA.Z
  , isomorphismGroup  :: V.Vector Orbit
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON GraphInvariant where
  toEncoding = genericToEncoding defaultOptions
