{-# LANGUAGE FlexibleInstances #-}
module Data.Graph.Invariant.Util
  ( hashNub
  , hashNubBy
  , minGroupBy
  ) where

import           Control.Monad.ST
import           Data.Foldable
import qualified Data.HashTable.ST.Basic       as HT
import           Data.Hashable                  ( Hashable(..) )

data ArgMin' a m = ArgMin' a m
  deriving (Eq, Ord, Show, Read)

instance (Ord a, Semigroup m) => Semigroup (ArgMin' a m) where
  x1@(ArgMin' k1 m1) <> x2@(ArgMin' k2 m2) = case compare k1 k2 of
    LT -> x1
    GT -> x2
    EQ -> ArgMin' k1 (m1 <> m2)
instance (Ord a, Monoid m) => Monoid (ArgMin' (Maybe a) m) where
  mempty = ArgMin' Nothing mempty

-- | Collects all values with the minimum key value.
minGroupBy :: (Ord a, Foldable t) => (b -> a) -> t b -> Maybe (a, [b])
minGroupBy f xs = case foldMap (\x -> ArgMin' (Just $ f x) [x]) xs of
  ArgMin' (Just k) ys -> Just (k, reverse ys)
  _                   -> Nothing

-- * Hashable

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
