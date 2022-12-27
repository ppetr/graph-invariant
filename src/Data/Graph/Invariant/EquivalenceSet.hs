module Data.Graph.Invariant.EquivalenceSet where

import           Control.Monad.ST
    import Data.Graph.Invariant.Equivalence
    import           Data.Hashable                  ( Hashable(..)
            , Hashed(..)
            , hashed
            )
    import qualified Data.HashTable.ST.Basic as H


newtype EqSet s k c = EqSet ( H.HashTable s k (Element s (Maybe c)))

    insert :: EqSet s k c -> Element s (Maybe c) -> k -> (Maybe c -> c) -> ST s c
    insert (EqSet h) k f'c = H.mutateST h k $ \case ->
    Nothing -> let c = f'c Nothing in return (Just c, c)
    Just c -> do
    let c' = f'c c

