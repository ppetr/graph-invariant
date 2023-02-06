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
{-# LANGUAGE DataKinds, FlexibleContexts, GeneralizedNewtypeDeriving, RankNTypes, TypeFamilies #-}
{-
 TODO:
 * Use Vector/Bundle instead converting to a list where appropriate.
 -}
module Data.Graph.Invariant.Perfect
  ( InvariantMonad
  , pseudoRandom
  , Algebra(..)
  , canonicalColoring
  ) where

import           Control.Arrow                  ( second )
import           Control.Exception              ( assert )
import           Control.Monad
import           Control.Monad.Coroutine
import           Control.Monad.Coroutine.SuspensionFunctors
                                                ( Yield(..) )
import           Control.Monad.Logic
import           Control.Monad.Reader           ( ReaderT(..) )
import           Control.Monad.Reader.Class
import           Control.Monad.ST
import           Control.Monad.ST.Class
import           Control.Monad.Trans.Class      ( lift )
import qualified Data.Foldable                 as F
import           Data.Function                  ( on )
import qualified Data.Graph.Invariant.Equivalence
                                               as E
import           Data.Graph.Invariant.Types
import           Data.Hashable                  ( Hashable(..)
                                                , Hashed
                                                , hashed
                                                )
import qualified Data.IntSet                   as IS
import           Data.List                      ( sortBy )
import qualified Data.List.NonEmpty            as NE
import qualified Data.Map                      as M
import           Data.Maybe                     ( fromJust
                                                , mapMaybe
                                                )
import           Data.STRef
import           Data.Sequence                  ( Seq
                                                , (|>)
                                                )
import           Data.Tuple                     ( swap )
import qualified Data.Vector                   as V
import qualified Data.Vector.Algorithms.Intro  as V
import qualified Data.Vector.Fusion.Bundle.Monadic
                                               as VB
import qualified Data.Vector.Generic.Mutable   as VM
                                                ( unsafeNew
                                                , unsafeWrite
                                                )
import qualified Data.Vector.Generic.New       as VN
                                                ( run
                                                , unstream
                                                )
import qualified Data.Vector.Storable          as VS
import qualified Data.Vector.Unboxed           as VU
import           Data.Word                      ( Word32 )
import           Numeric.LinearAlgebra          ( Vector
                                                , Z
                                                , accum
                                                , konst
                                                , toList
                                                )
import qualified System.Random.MWC             as MWC

newtype InvariantMonad s a = InvariantMonad (ReaderT (MWC.GenST s) (ST s) a)
  deriving (Functor, Applicative, Monad)

data Seed = Seed !MWC.Seed !(VU.Vector Word32)

instance Eq Seed where
  (Seed s1 _) == (Seed s2 _) = s1 == s2
instance Show Seed where
  showsPrec n (Seed s _) = showsPrec n s
instance Ord Seed where
  compare (Seed _ v1) (Seed _ v2) = compare v1 v2

seed :: MWC.Seed -> Seed
seed s = Seed s (MWC.fromSeed s)

initialSeed :: Seed
initialSeed = seed (MWC.toSeed V.empty)

runInvariantM :: (forall s . InvariantMonad s a) -> Seed -> (a, Seed)
runInvariantM k (Seed s _) = runST $ do
  r  <- MWC.restore s
  x  <- let InvariantMonad k' = k in runReaderT k' r
  r' <- MWC.save r
  return (x, seed r')

pseudoRandom :: InvariantMonad s F
pseudoRandom =
  InvariantMonad $ ReaderT (fmap fromIntegral . MWC.uniformR (2, maxBoundF))

twist :: InvariantMonad s (F -> F)
twist = (\a b x -> a * x + b) <$> pseudoRandom <*> pseudoRandom

sortAndTwistV :: Vector F -> InvariantMonad s (Vector F)
sortAndTwistV =
  VS.mapM (\x -> fmap ($ x) twist) <=< InvariantMonad . lift . sortV

sortV :: Vector F -> ST s (Vector F)
sortV v = do
  v' <- VS.thaw v
  V.sort v'
  VS.unsafeFreeze v'
{-# INLINE sortV #-}

data Algebra = Algebra Int
                       (forall s . Vector F -> InvariantMonad s (Vector F))
                       (Vector Int -> Bool)

groupsBySize :: (Ord a) => [(a, Int)] -> [(a, IS.IntSet)]
groupsBySize =
  sortBy (on compare (\(x, js) -> (IS.size js, x)))
    . M.toList
    . M.fromListWith (<>)
    . map (\(x, j) -> (x, IS.singleton j))

-- | Returns the group that is (1) smallest in size, but with at least 2
-- elements, and (2) has the minimum invariant. Therefore a `Just` has always
-- at least 2 elements.
findSmallest2Group :: (Ord a) => [a] -> Maybe (a, IS.IntSet)
findSmallest2Group =
  F.find (\(_, js) -> IS.size js >= 2) . groupsBySize . (`zip` [0 ..])

newtype HashedVector = HashedVector (Vector F)
  deriving (Eq, Ord, Show)

instance Hashable HashedVector where
  hashWithSalt s (HashedVector v) =
    hashWithSalt s . map (fromIntegral :: F -> Z) . VS.toList $ v

-- | `SortedColoring` represents one particular way how to color a structure.
-- Reaching the same coloring in two different ways is a witness of an
-- isomorphism.
newtype SortedColoring = SortedColoring (Hashed HashedVector)
  deriving (Eq, Ord, Show, Hashable)

coloring :: Vector F -> SortedColoring
coloring v = SortedColoring (hashed . HashedVector $ runST (sortV v))

permutation :: Vector F -> Vector F -> Vector Int
permutation u v = VS.create $ do
  let n = VS.length u
  p <- assert (n == VS.length v) $ VM.unsafeNew n
  V.zipWithM_ (\(_, j) (_, k) -> VM.unsafeWrite p j k)
              (orderIndices u)
              (orderIndices v)
  return p
 where
  orderIndices :: Vector F -> V.Vector (F, Int)
  orderIndices x = V.create $ do
    ix <-
      VN.run
      . VN.unstream
      . VB.reVector
      . VB.map swap
      . VB.indexed
      . VB.fromVector
      $ x
    V.sort ix
    return ix
  {-# INLINE orderIndices #-}

updateIndex :: Int -> F -> Vector F -> Vector F
updateIndex w x v = updateIndices const v [(w, x)]
{-# INLINE updateIndex #-}

updateIndices
  :: (Integral a)
  => (F -> F -> F) -- ^ twists `fromIntegral x` and a previous value
  -> Vector F
  -> [(Int, a)]
  -> Vector F
updateIndices f v ws = accum v f (map (\(j, x) -> (j, fromIntegral x)) ws)
{-# INLINE updateIndices #-}

addToResult
  :: (Vector Int -> Bool)
  -> NE.NonEmpty (Vector F)
  -> Vector F
  -> (NE.NonEmpty (Vector F), Maybe (Vector Int))
addToResult is_iso vs v
  | (r : _) <- mapMaybe (mfilter is_iso . Just . (`permutation` v))
                        (F.toList vs)
  = (vs, Just r)
  | otherwise
  = (v NE.<| vs, Nothing)

coroutineZero :: (MonadPlus m) => Coroutine s m a
coroutineZero = Coroutine mzero

coroutinePlus
  :: (MonadPlus m) => Coroutine s m a -> Coroutine s m a -> Coroutine s m a
coroutinePlus x y = Coroutine (resume x `mplus` resume y)

coroutineSum
  :: (MonadPlus m, Foldable f, Functor f)
  => f (Coroutine s m a)
  -> Coroutine s m a
coroutineSum = Coroutine . msum . fmap resume

canonicalColoringStep
  :: Vector F
  -> Seed
  -> Coroutine
       (Yield SortedColoring)
       ( LogicT
           (ReaderT (Algebra, V.Vector (E.Element s IS.IntSet)) (ST s))
       )
       (Vector F)
canonicalColoringStep v seed = do
  (Algebra _ f _, eq) <- lift ask
  let ((v', twist'i), seed') = runInvariantM ((,) <$> f v <*> twist) seed
  suspend . Yield (coloring v') $ case findSmallest2Group (toList v') of
    Nothing      -> return v'
    Just (i, ws) -> do
      let i' = twist'i i
      visited <- liftST $ newSTRef IS.empty
      coroutineSum . (`map` IS.toList ws) $ \w -> do
        unprocessed <- liftST $ do
          vs    <- readSTRef visited
          w_set <- IS.intersection ws <$> E.find (eq V.! w)
          let vs' = vs `IS.union` w_set
          writeSTRef visited vs'
          return (IS.size w_set + IS.size vs == IS.size vs')
        if unprocessed
          then canonicalColoringStep (updateIndex w i' v') seed'
          else coroutineZero
{-# INLINE canonicalColoringStep #-}

data StepResult s
  = Result !(NE.NonEmpty (Vector F)) !(Seq (Vector Int))
    | ColoringStep !SortedColoring !(Coroutine
       (Yield SortedColoring)
       ( LogicT
           (ReaderT (Algebra, V.Vector (E.Element s IS.IntSet)) (ST s))
       )
       (Vector F))

mergeSteps
  :: (Vector Int -> Bool) -> StepResult s -> StepResult s -> StepResult s
mergeSteps _ r@Result{}           (ColoringStep _ _)   = r
mergeSteps _ (  ColoringStep _ _) r@Result{}           = r
mergeSteps _ r@(ColoringStep c k) s@(ColoringStep d l) = case compare c d of
  LT -> r
  EQ -> ColoringStep d (l `coroutinePlus` k)
  GT -> s
mergeSteps f'iso (Result us ps) (Result vs qs) = uncurry Result
  $ foldr addTo (us, ps <> qs) vs
 where
  addTo v (a'us, rs) = second (maybe rs (rs |>)) (addToResult f'iso a'us v)
{-# INLINE mergeSteps #-}

runStepLevel
  :: Coroutine
       (Yield SortedColoring)
       (LogicT (ReaderT (Algebra, V.Vector (E.Element s IS.IntSet)) (ST s)))
       (Vector F)
  -> ReaderT
       (Algebra, V.Vector (E.Element s IS.IntSet))
       (ST s)
       (StepResult s)
runStepLevel k = fromJust <$> runLogicT (resume k) addStepM (return Nothing)
 where
  -- toStep :: Either (Yield SortedColoring a) (Vector F) -> StepResult s
  toStep (Left  (Yield c l)) = ColoringStep c l
  toStep (Right v          ) = Result (v NE.:| []) mempty
  addStepM e m'r = do
    (Algebra _ _ f'iso, _) <- ask
    let s = toStep e
    Just . maybe s (mergeSteps f'iso s) <$> m'r
{-# INLINE runStepLevel #-}

iterateStepLevels
  :: Coroutine
       (Yield SortedColoring)
       (LogicT (ReaderT (Algebra, V.Vector (E.Element s IS.IntSet)) (ST s)))
       (Vector F)
  -> ReaderT
       (Algebra, V.Vector (E.Element s IS.IntSet))
       (ST s)
       (NE.NonEmpty (Vector F), Seq (Vector Int))
iterateStepLevels = runStepLevel >=> unpack
 where
  unpack (Result       vs ps) = return (vs, ps)
  unpack (ColoringStep _  l ) = iterateStepLevels l
{-# INLINE iterateStepLevels #-}

totalInvariant :: Seed -> Vector F -> F
totalInvariant s is = VS.sum . fst $ runInvariantM (sortAndTwistV is) s

canonicalColoring
  :: (forall s . InvariantMonad s Algebra)
  -> (F, NE.NonEmpty (Vector F), Seq (Vector Int))
canonicalColoring k
  | (a@(Algebra n _ _), seed0) <- runInvariantM k initialSeed
  , n > 0
  = let (is@(i NE.:| _), ps) = runST $ do
          eq <- V.generateM n (E.newElement . IS.singleton)
          runReaderT
            (iterateStepLevels (canonicalColoringStep (konst 1 n) seed0))
            (a, eq)
    in  (fromIntegral (NE.length is) * totalInvariant seed0 i, is, ps)
  | otherwise
  = (0, VS.empty NE.:| [], mempty)
