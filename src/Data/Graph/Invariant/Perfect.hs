{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, LambdaCase, RankNTypes, TypeFamilies #-}
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

import           Control.Exception              ( assert )
import           Control.Monad.Logic
import           Control.Monad.Reader
import           Control.Monad.ST
import qualified Data.Foldable                 as F
import           Data.Function                  ( on )
import           Data.Functor                   ( (<&>) )
import qualified Data.Graph.Invariant.Equivalence
                                               as E
import           Data.Graph.Invariant.Types
import qualified Data.HashTable.ST.Basic       as HT
import           Data.Hashable                  ( Hashable(..)
                                                , Hashed
                                                , hashed
                                                )
import qualified Data.IntSet                   as IS
import           Data.List                      ( sortBy )
import qualified Data.Map                      as M
import           Data.Maybe                     ( fromJust
                                                , isJust
                                                )
import           Data.Monoid
import           Data.Tuple                     ( swap )
import qualified Data.Vector                   as V
import qualified Data.Vector.Algorithms.Intro  as V
import qualified Data.Vector.Fusion.Bundle.Monadic
                                               as VB
import qualified Data.Vector.Generic           as VG
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
import           Debug.Trace
import           Numeric.LinearAlgebra          ( Vector
                                                , Z
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

mergePermutationImage
  :: (Eq c, Monoid c) => V.Vector (E.Element s c) -> Vector Int -> ST s ()
mergePermutationImage es =
  VG.imapM_ $ \j k -> when (j /= k) (void $ E.union (<>) (es V.! j) (es V.! k))
{-# INLINE mergePermutationImage #-}

iterateInvariant :: Algebra -> Seed -> Vector F -> ((Vector F, F -> F), Seed)
iterateInvariant (Algebra _ f _) s v = runInvariantM ((,) <$> f v <*> twist) s

updateIndex :: Int -> F -> Vector F -> Vector F
updateIndex w x v = accum v const [(w, x)]

anyColoring :: Algebra -> Seed -> Vector F -> (Vector F, [SortedColoring])
anyColoring a = loop []
 where
  loop cs s v =
    let ((v', twist'i), s') = iterateInvariant a s v
        cs'                 = coloring v' : cs
    in  case findSmallest2Group (toList v') of
          Just (i, ws) | (w : _) <- IS.toList ws ->
            loop cs' s' (updateIndex w (twist'i i) v')
          _ -> (v', cs')

matchColoring
  :: (MonadPlus m)
  => Algebra
  -> (SortedColoring -> m Bool)
  -> Seed
  -> Vector F
  -> m (Vector F, SortedColoring)
matchColoring a f'c = loop
 where
  loop s v = do
    let ((v', twist'i), s') = iterateInvariant a s v
        c                   = coloring v'
    guard =<< f'c c
    case findSmallest2Group (toList v') of
      Nothing -> return (v', c)
      Just (i, ws) ->
        let i' = twist'i i
        in  msum . map (\w -> loop s' (updateIndex w i' v')) $ IS.toList ws
{-# INLINE matchColoring #-}

data EqSet = EqSet SortedColoring Int (Vector F) [Vector Int]
  deriving (Eq, Ord, Show)

canonicalColoringStep :: Algebra -> Seed -> Vector F -> (Vector F, Seed)
canonicalColoringStep a@(Algebra n _ f'iso) s v = runST $ do
  let ((v', twist'i), s') = iterateInvariant a s v
  case findSmallest2Group (toList v') of
    Nothing      -> return (v', s')
    Just (i, ws) -> do
      let i' = twist'i i
      eq  <- V.replicateM n (E.newElement mempty)
      cs  <- HT.new
      c2e <- HT.new
      forM_ (IS.toList ws) $ \w -> E.find (eq V.! w) >>= \case
        First (Just _) -> traceEvent "Found equivalent coloring" $ return ()
        _              -> do
          let v'' = updateIndex w i' v'
          runLogicT
              (matchColoring a (fmap isJust . lift . HT.lookup cs) s' v'')
              (\(z, c) cont -> do
                e'                              <- fromJust <$> HT.lookup c2e c
                First (Just (EqSet c' w' u ps)) <- E.find e'
                assert (c == c') $ return ()
                let p = permutation u z
                if f'iso p
                  then do
                    traceEvent "Found coloring for index" $ return ()
                    E.set (First (Just (EqSet c' w' u (p : ps)))) e'
                    mergePermutationImage eq p
                  else traceEvent "Coloring failed isomorphism check" cont
              )
            $ do -- No match, this is a new class.
                let (u, cs') = anyColoring a s' v''
                    c        = coloring u
                traceEvent "Computed new coloring" $ return ()
                let e = eq V.! w
                E.set (First (Just $ EqSet c w u [])) e
                HT.insert c2e c e
                forM_ cs' (\d -> HT.insert cs d ())
      -- Now we have computed all classes. Find the smallest one and compute it.
      gs <- fmap (map snd . groupsBySize) . forM (IS.toList ws) $ \w -> do
        First (Just (EqSet c _ _ _)) <- E.find (eq V.! w)
        return (c, w)
      -- It can still be that we have more classes that have the same minimum size.
      -- Then we have to compute all of them and take the sorted minimum as the
      -- invariant, since we don't have another coordinate-independent way how
      -- to tell them apart.
      let min_size = IS.size (head gs)
          is       = takeWhile (\g -> IS.size g == min_size) gs <&> \g ->
            let w0  = IS.findMin g
                v'' = updateIndex w0 i' v'
                -- TODO: Re-use equivalence classes from each run in the
                -- following ones.
                i's = canonicalColoringStep a s' v''
            in  (coloring (fst i's), w0, i's)
      let (_, _, i'r) = F.minimum is
      -- TODO: Emit the orbits.
      -- First (Just (_, _, ps)) <- E.find (eq V.! w'r)
      traceEvent "Return invariants" $ return i'r

totalInvariant :: Seed -> Vector F -> F
totalInvariant s is = VS.sum . fst $ runInvariantM (sortAndTwistV is) s

canonicalColoring :: (forall s . InvariantMonad s Algebra) -> (F, Vector F)
canonicalColoring k =
  let (a@(Algebra n _ _), seed0) = runInvariantM k initialSeed
      (is               , seed1) = canonicalColoringStep a seed0 (konst 1 n)
  in  (totalInvariant seed1 is, is)
