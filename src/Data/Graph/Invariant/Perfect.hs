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

import           Control.Applicative
import           Control.Exception              ( assert )
import           Control.Monad
import           Control.Monad.Reader           ( ReaderT(..) )
import           Control.Monad.Reader.Class
import           Control.Monad.ST
import           Control.Monad.ST.Class
import           Control.Monad.State.Class
import           Control.Monad.Trans.Class      ( lift )
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.RWS.Strict ( RWST(..)
                                                , execRWST
                                                )
import           Control.Monad.Writer.Class
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
import           Data.Monoid
import           Data.Sequence                  ( Seq(..)
                                                , singleton
                                                )
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
  :: (Semigroup c) => V.Vector (E.Element s c) -> Vector Int -> ST s ()
mergePermutationImage es =
  VG.imapM_ $ \j k -> when (j /= k) (void $ E.union (<>) (es V.! j) (es V.! k))
{-# INLINE mergePermutationImage #-}

updateIndex :: Int -> F -> Vector F -> Vector F
updateIndex w x v = updateIndices const v [(w, x)]

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

data LeastChain = Unknown | Result (NE.NonEmpty (Vector F)) | ColoringStep SortedColoring LeastChain

chainResult :: LeastChain -> Maybe (NE.NonEmpty (Vector F))
chainResult Unknown                = Nothing
chainResult (Result vs           ) = Just vs
chainResult (ColoringStep _ chain) = chainResult chain

descend
  :: (MonadState LeastChain m, MonadPlus m) => SortedColoring -> m a -> m a
descend c k = do
  chain <- get
  case chain of
    Result{}               -> empty
    ColoringStep c' chain' -> case compare c' c of
      LT -> empty
      EQ -> put chain'
      GT -> put Unknown  -- discard the current chain
    Unknown -> put Unknown
  x <- k
  modify (ColoringStep c)
  return x
{-# INLINE descend #-}

-- | Extracts a value in an `Alternative`.
extractAlt :: (Alternative m) => m a -> m (Maybe a)
extractAlt k = (Just <$> k) <|> pure Nothing
{-# INLINE extractAlt #-}

canonicalColoringStep
  :: Vector F
  -> Seed
  -> MaybeT
       (RWST Algebra (Seq (Vector Int)) LeastChain (ST s))
       (V.Vector (E.Element s Any))
canonicalColoringStep v seed = do
  (Algebra n f f'iso) <- ask
  let ((v', twist'i), seed') = runInvariantM ((,) <$> f v <*> twist) seed
  descend (coloring v') $ do
    eq <- liftST $ V.replicateM n (E.newElement mempty)
    case findSmallest2Group (toList v') of
      Nothing -> do
        chain <- get
        case chain of
          Result vs | (vs', m'p) <- addToResult f'iso vs v' -> do
            put (Result vs')
            case m'p of
              Just p -> do
                tell (singleton p)
                liftST $ mergePermutationImage eq p
              Nothing -> return ()
          _ -> put (Result (v' NE.:| []))
      Just (i, ws) -> do
        let i' = twist'i i
        forM_ (IS.toList ws) $ \w -> do
          (Any done) <- liftST $ E.find (eq V.! w)
          unless done $ do
            m'eq' <- extractAlt
              $ canonicalColoringStep (updateIndex w i' v') seed'
            liftST $ E.set (Any True) (eq V.! w)
            case m'eq' of
              Just eq' -> liftST $ do
                VG.forM_ eq' $ E.set mempty
                VG.zipWithM_ (E.union (<>)) eq eq'
              Nothing -> return ()
    return eq
{-# INLINE canonicalColoringStep #-}

totalInvariant :: Seed -> Vector F -> F
totalInvariant s is = VS.sum . fst $ runInvariantM (sortAndTwistV is) s

canonicalColoring
  :: (forall s . InvariantMonad s Algebra)
  -> (F, NE.NonEmpty (Vector F), Seq (Vector Int))
canonicalColoring k =
  let (a@(Algebra n _ _), seed0) = runInvariantM k initialSeed
      (chain, ps) =
        runST
          (execRWST (runMaybeT (canonicalColoringStep (konst 1 n) seed0))
                    a
                    Unknown
          )
      is@(i NE.:| _) = fromJust (chainResult chain)
  in  (fromIntegral (NE.length is) * totalInvariant seed0 i, is, ps)
