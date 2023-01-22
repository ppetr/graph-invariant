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
{-# LANGUAGE OverloadedStrings #-}
module Data.Graph.Dimacs.Parse
  ( ColoredGraph(..)
  , cgSize
  , parseColored
  , undirected
  ) where

import           Control.Applicative
import           Data.Array
import           Data.Attoparsec.Text          as A
import           Data.Graph
import qualified Data.Text                     as T

import           Data.Graph.Invariant.Types
import           Data.Graph.Invariant.Util

-- | Parses the _bliss_ (DIMACS textual graph file format) described in
-- http://www.tcs.hut.fi/Software/bliss/fileformat.shtml.
parseColored :: Parser ColoredGraph
parseColored = do
  txt                <- many comment
  (n'nodes, n'edges) <- problem
  let bs = (1, n'nodes)
  cs <- accumArray (\_ x -> x) 0 bs <$> many (color n'nodes)
  g  <- buildG bs <$> count n'edges (edge n'nodes)
  return $ ColoredGraph g cs (T.strip $ T.unlines txt)
 where
  space' = skipMany1 (skip isHorizontalSpace)
  vertex n = decimal >>= \v -> if (v < 1) || (v > n)
    then fail "Vertex number out of bounds"
    else return v
  comment = char 'c' *> A.takeWhile (not . isEndOfLine) <* endOfLine
  -- Returns the number of vertices and the number of edges.
  problem =
    (,)
      <$> (char 'p' *> space' *> string "edge" *> space' *> decimal)
      <*> (space' *> decimal <* endOfLine)
  color n =
    (,)
      <$> (char 'n' *> space' *> vertex n)
      <*> (space' *> decimal <* endOfLine)
  edge n =
    (,)
      <$> (char 'e' *> space' *> vertex n)
      <*> (space' *> vertex n <* endOfLine)

undirected :: Graph -> Graph
undirected g = buildG (bounds g) (hashNub . concatMap edge $ edges g)
  where edge (i, j) = [(i, j), (j, i)]
