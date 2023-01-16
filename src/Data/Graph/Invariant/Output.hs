{-# LANGUAGE DeriveGeneric #-}
module Data.Graph.Invariant.Output
  ( GraphInvariant(..)
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

data GraphInvariant = GraphInvariant
  { name                  :: Maybe Text
  , invariantVersion      :: Text
  , invariant             :: LA.Z
  , elementInvariants     :: NonEmpty (VS.Vector LA.Z)
  , isomorphismGenerators :: Seq (VS.Vector Int)
  , runStats              :: Maybe RunStats
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON GraphInvariant where
  toEncoding = genericToEncoding defaultOptions
