{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, LambdaCase, RankNTypes, TypeFamilies #-}
module Data.Graph.Invariant.Perfect
  ( InvariantMonad
  , pseudoRandom
  , twist
  , twistAndSortV
  , Algebra(..)
  , canonicalColoring
  ) where

import           Control.Exception              ( assert )
import           Control.Monad                  ( void )
import           Control.Monad.Reader
import           Control.Monad.ST
import qualified Data.Foldable                 as F
import           Data.Function                  ( on )
import qualified Data.Graph.Invariant.Equivalence
                                               as E
import           Data.Graph.Invariant.Types
import qualified Data.IntSet                   as IS
import           Data.List                      ( sortBy )
import qualified Data.Map                      as M
import           Data.STRef
import qualified Data.Vector                   as V
import qualified Data.Vector.Algorithms.Intro  as V
import qualified Data.Vector.Storable          as VS
import qualified Data.Vector.Unboxed           as VU
import           Data.Word                      ( Word32 )
import           Debug.Trace
import           Numeric.LinearAlgebra          ( Vector
                                                , accum
                                                , konst
                                                , toList
                                                )
import qualified System.Random.MWC             as MWC

newtype InvariantMonad s a = InvariantMonad (ReaderT (MWC.GenST s) (ST s) a)
  deriving (Functor, Applicative, Monad)

data Seed = Seed MWC.Seed (VU.Vector Word32)

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

twistAndSortV :: Vector F -> InvariantMonad s (Vector F)
twistAndSortV =
  InvariantMonad . lift . sortV <=< VS.mapM (\x -> fmap ($ x) twist)

sortV :: Vector F -> ST s (Vector F)
sortV v = do
  v' <- VS.thaw v
  V.sort v'
  VS.unsafeFreeze v'

data Algebra = Algebra Int
                       (forall s . Vector F -> InvariantMonad s (Vector F))

groupsBySize :: (Ord a) => [a] -> [(a, IS.IntSet)]
groupsBySize =
  sortBy (on compare (\(x, js) -> (IS.size js, x)))
    . M.toList
    . M.fromListWith (<>)
    . zipWith (\j x -> (x, IS.singleton j)) [0 ..]

-- | Returns the group that is (1) smallest in size, but with at least 2
-- elements, and (2) has the minimum invariant. Therefore a `Just` has always
-- at least 2 elements.
findSmallest2Group :: (Ord a) => [a] -> Maybe (a, IS.IntSet)
findSmallest2Group = F.find (\(_, js) -> IS.size js >= 2) . groupsBySize

canonicalColoringStep
  :: Algebra
  -> Seed
  -> Vector F
  -> ST
       s
       ( (Vector F, Seed)
       , V.Vector (E.Element s (Maybe (Vector F, Seed)))
       )
canonicalColoringStep a@(Algebra n f) s vs = do
  let ((vs', twist'i), s') = runInvariantM ((,) <$> f vs <*> twist) s
  eq <- V.replicateM n (E.newElement Nothing)
  case findSmallest2Group (toList vs') of
    Nothing      -> (\vs'' -> ((vs'', s'), eq)) <$> sortV vs'
    Just (i, ws) -> do
      let i' = twist'i i
      i2eq <- newSTRef M.empty
      is   <- forM (IS.toList ws) $ \w ->
        let e = eq V.! w
        in
          E.find (eq V.! w) >>= \case
            (Just i's) -> do
              traceM $ "Found equivalent value for index " ++ show w
              return i's
            Nothing -> do
              traceM $ "Computing value for index " ++ show w
              let vs'' = accum vs' const [(w, i')]
              (i's, eq') <- assert (vs' /= vs'')
                $ canonicalColoringStep a s' vs''
              -- Reset invariant values. We only want to keep the sets without
              -- invariants (which aren't valid on this upper level).
              V.forM_ eq' $ E.set Nothing
              E.set (Just i's) e
              -- Merge the elements determined to be isomorphic by the nested call,
              -- but keep the invariants (or the lack of them) only from the
              -- current run.
              Just _ <- E.find e
              V.zipWithM_ (E.union mplus) eq eq'
              Just _ <- E.find e
              -- Update the map.
              m      <- readSTRef i2eq
              case M.lookup (fst i's) m of
                Nothing -> writeSTRef i2eq (M.insert (fst i's) e m)
                Just e' -> void $ E.union mplus e' e
              traceM $ "Invariant for index " ++ show w ++ ": " ++ show (fst i's)
              return i's
      let ((i's, _) : _) = groupsBySize is
      return (i's, eq)

canonicalColoring :: (forall s . InvariantMonad s Algebra) -> (F, Vector F)
canonicalColoring k = runST $ do
  let (a@(Algebra n _), seed0) = runInvariantM k initialSeed
  ((is, seed1), _) <- canonicalColoringStep a seed0 (konst 1 n)
  return (VS.sum . fst $ runInvariantM (twistAndSortV is) seed1, is)
