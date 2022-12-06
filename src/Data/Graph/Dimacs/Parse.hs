{-# LANGUAGE OverloadedStrings #-}
module Data.Graph.Dimacs.Parse
  ( ColoredGraph(..)
  , cgSize
  , parseColored
  , undirected
  ) where

import           Control.Applicative
import           Data.Array
import           Data.Attoparsec.Text
import           Data.Foldable                  ( concatMap )
import           Data.Graph
import qualified Data.HashSet                  as HS

data ColoredGraph = ColoredGraph
  { cgGraph  :: Graph
  , cgColors :: Array Vertex Int
  -- ^ Has the same bounds as the `Graph`.
  }
  deriving (Eq, Ord, Show, Read)

cgSize :: ColoredGraph -> Int
cgSize (ColoredGraph g _) = let (mn, mx) = bounds g in mx - mn + 1

-- | Parses the _bliss_ (DIMACS textual graph file format) described in
-- http://www.tcs.hut.fi/Software/bliss/fileformat.shtml.
parseColored :: Parser ColoredGraph
parseColored = do
  _                  <- many comment
  (n'nodes, n'edges) <- problem
  let bs = (1, n'nodes)
  cs <- accumArray (\_ x -> x) 0 bs <$> many (color n'nodes)
  g  <- buildG bs <$> count n'edges (edge n'nodes)
  return $ ColoredGraph g cs
 where
  space' = skipMany1 (skip isHorizontalSpace)
  vertex n = decimal >>= \v -> if (v < 1) || (v > n)
    then fail "Vertex number out of bounds"
    else return v
  comment = char 'c' *> skipWhile (not . isEndOfLine) *> endOfLine
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
undirected g = buildG (bounds g)
                      (HS.toList . HS.fromList . concatMap edge $ edges g)
  where edge (i, j) = [(i, j), (j, i)]
