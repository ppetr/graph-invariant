module Data.Graph.Invariant.Equivalence
  ( Element
  , newElement
  , debugPrint
  , find
  , modify
  , same
  , set
  , union
  ) where

import           Control.Monad.ST
import           Data.STRef
import           Data.Word                      ( Word8 )

newtype Element s c = Element (STRef s (Node s c))

debugPrint :: (Show c) => Element s c -> ST s String
debugPrint (Element e) = do
  n <- readSTRef e
  p <- f n
  return $ "Element(" ++ p ++ ")"
 where
  f (Repr r c) = return $ "Repr{rank=" ++ show r ++ ",class=" ++ show c ++ "}"
  f (Node n  ) = do
    n' <- readSTRef n
    p  <- f n'
    return $ "Node(" ++ p ++ ")"

-- | A node of the Union-Find algorithm.
--
-- Since we guarantee that `Node i` is always a valid number (never
-- constructed, only copied/replaced), we can use faster unsafe operations
-- https://wiki.haskell.org/Arrays#Unsafe_indexing.2C_freezing.2Fthawing.2C_running_over_array_elements.
data Node s c = Node {-# UNPACK #-} !(STRef s (Node s c))
  | Repr {-# UNPACK #-} !Word8 c

newElement :: c -> ST s (Element s c)
newElement = fmap Element . newSTRef . Repr 0

find :: Element s c -> ST s c
find = fmap (\(_, _, c) -> c) . find'

find' :: Element s c -> ST s (STRef s (Node s c), Word8, c)
find' (Element i0) = readSTRef i0 >>= loop i0
 where
  loop i (Repr r c) = return (i, r, c)
  loop i (Node p  ) = do
    o <- readSTRef p
    case o of
      Node _ -> writeSTRef i o
      _      -> return ()
    loop p o

same :: (Eq c) => Element s c -> Element s c -> ST s Bool
same i j = (==) <$> find i <*> find j

union
  :: (Eq c) => (c -> c -> c) -> Element s c -> Element s c -> ST s (Maybe c)
union f = union' (\x y -> if x == y then Nothing else Just (f x y))

union'
  :: (c -> c -> Maybe c)
  -- ^ Function that compares the classes of the two elements. If it returns
  -- `Nothing`, elements are considered equal. Otherwise its `Just` result will
  -- be the class of the merged sets.
  -> Element s c
  -> Element s c
  -> ST s (Maybe c)
union' f x y = do
  (i, r, c) <- find' x
  (j, s, d) <- find' y
  let m'cd = f c d
  case m'cd of
    Nothing -> return ()
    Just cd -> case compare r s of
      LT -> writeSTRef i (Node j) >> writeSTRef j (Repr s cd)
      GT -> writeSTRef j (Node i) >> writeSTRef i (Repr r cd)
      EQ -> writeSTRef i (Node j) >> writeSTRef j (Repr (s + 1) cd)
  return m'cd

set :: c -> Element s c -> ST s ()
set d = modify (const d)

modify :: (c -> c) -> Element s c -> ST s ()
modify f x = do
  (i, r, c) <- find' x
  writeSTRef i (Repr r (f c))
