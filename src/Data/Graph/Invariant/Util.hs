module Data.Graph.Invariant.Util
  ( hashNub
  , hashNubBy
  ) where

import           Control.Monad.ST
import           Data.Foldable
import qualified Data.HashTable.ST.Basic       as HT
import           Data.Hashable                  ( Hashable(..) )

-- | Returns unique elements of a collection in arbitrary order,
hashNubBy :: (Foldable t, Eq b, Hashable b) => (a -> b) -> t a -> [a]
hashNubBy f xs = runST $ do
  ht <- HT.new
  forM_ xs $ \x -> HT.insert ht (f x) x
  HT.foldM (\ys (_, y) -> return (y : ys)) [] ht
{-# INLINE hashNubBy #-}

-- | Returns unique elements of a collection in arbitrary order,
hashNub :: (Foldable t, Eq a, Hashable a) => t a -> [a]
hashNub = hashNubBy id
{-# INLINE hashNub #-}
