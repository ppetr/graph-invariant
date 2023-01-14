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
import           Control.Monad.Reader
import           Control.Monad.ST
import           Control.Monad.ST.Class
import           Control.Monad.State
import           Control.Monad.Trans.Maybe
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
import qualified Data.Map                      as M
import           Data.Maybe                     ( fromJust )
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

composeP :: (VS.Storable a) => Vector a -> Vector Int -> Vector a
composeP p = VG.map (p VS.!)
{-# INLINE composeP #-}

data Orbit = Orbit (Vector F) [Vector Int]
  deriving (Eq, Ord, Show)

-- | The permutation that maps the vector of the
-- 1st argument to the vector of the 2nd is
-- intentionally the head of the permutation list
-- of the result so that it can be easily accessed.
instance Semigroup Orbit where
  (Orbit v qs) <> (Orbit u ps) =
    let r = permutation u v in Orbit u ([r] ++ map (`composeP` r) qs ++ ps)

mergePermutationImage
  :: (Eq c, Semigroup c) => V.Vector (E.Element s c) -> Vector Int -> ST s ()
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

-- | Given a new coloring and a set of permutations, searches whether the
-- coloring is an isomorphism of one of the `[Orbit]` results. If yes,
-- adds it to this permutations. If not, adds a new `Orbit` based on the
-- coloring.
addResult
  :: (Vector Int -> Bool) -> Orbit -> Vector F -> Maybe (Orbit, Vector Int)
addResult is_iso (Orbit u ps) v
  | r <- permutation u v, is_iso r = Just (Orbit u (r : ps), r)
  | otherwise                      = Nothing

data LeastChain = Unknown | Result Orbit [Vector F] | ColoringStep SortedColoring LeastChain

chainResult :: LeastChain -> Maybe (Orbit, [Vector F])
chainResult Unknown                = Nothing
chainResult (Result       o vs   ) = Just (o, vs)
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

data EqSet = EqSet
  { canonicalInvariant :: Maybe (Vector F)
  , permutations       :: Maybe Orbit
  }
  deriving (Eq, Ord, Show)

instance Semigroup EqSet where
  (EqSet u ps) <> (EqSet v qs) =
    EqSet (getFirst (First u <> First v)) (ps <> qs)

-- | Extracts a value in an `Alternative`.
extractAlt :: (Alternative m) => m a -> m (Maybe a)
extractAlt k = (Just <$> k) <|> pure Nothing

canonicalColoringStep
  :: Algebra
  -> Vector F
  -> Seed
  -> StateT LeastChain (MaybeT (ST s)) (V.Vector (E.Element s Any))
canonicalColoringStep a@(Algebra n f f'iso) v seed = do
  let ((v', twist'i), seed') = runInvariantM ((,) <$> f v <*> twist) seed
  descend (coloring v') $ do
    eq <- liftST $ V.replicateM n (E.newElement mempty)
    case findSmallest2Group (toList v') of
      Nothing -> do
        chain <- get
        let (chain', m'p) = case chain of
              Result o vs
                | Just (o', p) <- addResult f'iso o v' -> (Result o' vs, Just p)
                |
                          -- TODO: Check permutations of vs -> v'
                  otherwise -> (Result o (v' : vs), Nothing)
              _ -> (Result (Orbit v' []) [], Nothing)
        put chain'
        case m'p of
          Just p  -> liftST $ mergePermutationImage eq p
          Nothing -> return ()
      Just (i, ws) -> do
        let i' = twist'i i
        forM_ (IS.toList ws) $ \w -> do
          (Any done) <- liftST $ E.find (eq V.! w)
          unless done $ do
            m'eq' <- extractAlt
              $ canonicalColoringStep a (updateIndex w i' v') seed'
            liftST $ E.set (Any True) (eq V.! w)
            case m'eq' of
              Just eq' -> liftST $ VG.zipWithM_ (E.union const) eq eq'
              Nothing  -> return ()
    return eq

{-

canonicalColoringStep
  :: Algebra
  -> Seed
  -> LeastChain
  -> Vector F
  -> MaybeT (ST s) (Vector F, V.Vector (E.Element EqSet), Seed)
canonicalColoringStep = undefined
canonicalColoringStep a@(Algebra n f f'iso) s least v = do
  let ((v', twist'i, twist'g), s') =
        runInvariantM ((,,) <$> f v <*> twist <*> twist2) s
      v'color = coloring v'
  case least of
    Result       _ _     -> empty
    ColoringStep c chain -> undefined
  eq <- V.replicateM n (E.newElement $ EqSet IS.empty Nothing Nothing)
  case findSmallest2Group (toList v') of
    Nothing      -> return (v', eq, s')
    Just (i, ws) -> do
      let i' = twist'i i
      cs  <- HT.new
      c2e <- HT.new
      forM_ (IS.toList ws) $ \w -> E.find (eq V.! w) >>= \case
        EqSet { permutations = Just _ } ->
          traceEvent "Found equivalent coloring" $ return ()
        _ -> do
          let v'' = updateIndex w i' v'
          runLogicT
              (do
                let f'c = fmap isJust . lift . HT.lookup cs
                (z, c) <- matchColoring a f'c s' v''
                e'     <- msum . map return . fromMaybe [] =<< lift
                  (HT.lookup c2e c)
                eqs@EqSet { permutations = ps } <- lift (E.find e')
                let qs@(Orbit _ (q : _)) =
                      Orbit z [] <> fromJust ps
                guard $ f'iso q
                traceEvent "Found coloring for index" $ return ()
                E.set eqs e'
                mergePermutationImage eq q
              )
              (\_ _ -> return ())
            $ do -- No match, so this is a new class.
                let (u, cs') = anyColoring a s' v''
                    c        = coloring u
                traceEvent "Computed new coloring" $ return ()
                let e = eq V.! w
                E.set
                  (EqSet (IS.singleton w) Nothing (Just (Orbit u [])))
                  e
                HT.mutate c2e c (\es' -> (Just (e : fromMaybe [] es'), ()))
                forM_ cs' (\d -> HT.insert cs d ())
      -- Now we have computed all classes.
      -- Mark elements according their class's sizes. We can't distinguish
      -- classes of the same size, since there is no way of assigning an invariant
      -- value to each of them at this stage.
      v'' <- updateIndices twist'g v' <$> forM
        (IS.toList ws)
        (\w ->
          (\EqSet { indices = js } -> (w, IS.size js)) <$> E.find (eq V.! w)
        )
      -- Find the smallest classes and compute their invariants.
      undefined
      {-
      gs <- fmap (map snd . groupsBySize) . forM (IS.toList ws) $ \w -> do
        First (Just (EqSet c _ _ _ _)) <- E.find (eq V.! w)
        return (c, w)
      -- It can still be that we have more classes that have the same minimum size.
      -- Then we have to compute all of them and take the sorted minimum as the
      -- invariant, since we don't have another coordinate-independent way how
      -- to tell them apart.
      let min_size = IS.size (head gs)
      (_, i0, seed0) <-
        F.minimum
        <$> forM_ (takeWhile (\g -> IS.size g == min_size) gs)
        $   \g -> do
              let w0  = IS.findMin g
                  v'' = updateIndex w0 i' v'
              First (Just e@(EqSet _ _ _ m'i _)) <- E.find (eq V.! w0)
              case m'i of
                Just i'c -> return i'c
                Nothing  -> do
                  (i, eq', _) <- canonicalColoringStep a s' v''
                  E.set (First (Just e { eqsCanonicalInvariant = Just i }))
                        (eq V.! w0)
                  VG.zipWithM_ (E.union const) (eq, eq')
                  return (coloring i, i)
      -- TODO: Emit the orbits.
      traceEvent "Return invariants" $ return (i0, eq, seed0)
      -}
-}

totalInvariant :: Seed -> Vector F -> F
totalInvariant s is = VS.sum . fst $ runInvariantM (sortAndTwistV is) s

canonicalColoring :: (forall s . InvariantMonad s Algebra) -> (F, Vector F)
canonicalColoring k =
  let (a@(   Algebra n _ _), seed0) = runInvariantM k initialSeed
      (Orbit is           _, _    ) = fromJust . chainResult . fromJust $ runST
        (runMaybeT
          (execStateT (canonicalColoringStep a (konst 1 n) seed0) Unknown)
        )
  in  (totalInvariant seed0 is, is)
