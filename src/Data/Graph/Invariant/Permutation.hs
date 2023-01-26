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
{-# LANGUAGE FlexibleContexts #-}
module Data.Graph.Invariant.Permutation
  ( asCycles
  ) where

import           Control.Monad.ST               ( runST )
import           Data.STRef
import           Data.Sequence                  ( Seq(..)
                                                , (|>)
                                                )
import qualified Data.Vector.Fusion.Bundle     as VB
import qualified Data.Vector.Generic           as VG
import qualified Data.Vector.Generic.Mutable   as VG
                                                ( unsafeExchange )
import qualified Data.Vector.Storable          as VS

asCycles :: VS.Vector Int -> Seq (VS.Vector Int)
asCycles u = runST $ do
  v <- VG.thaw u
  r <- newSTRef mempty
  let cycle0 i _ = do
        j <- VG.unsafeExchange v i i
        if i == j then return () else cycle (VB.singleton (i + 1)) j
      cycle b i = do
        j <- VG.unsafeExchange v i i
        if i == j
          then modifySTRef r (|> VG.unstream b)
          else cycle (VB.snoc b (i + 1)) j
  VG.imapM_ cycle0 u
  readSTRef r
