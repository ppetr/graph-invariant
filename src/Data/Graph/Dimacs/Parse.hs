{-# LANGUAGE OverloadedStrings #-}
module Data.Graph.Dimacs.Parse
  ( ColoredGraph(..)
  , parseColored
  , undirected
  ) where

import           Control.Applicative
import           Data.Array                     ( bounds )
import           Data.Attoparsec.Text
import           Data.Foldable                  ( concatMap )
import           Data.Graph
import qualified Data.HashMap.Strict           as HM
import qualified Data.HashSet                  as HS

data ColoredGraph = ColoredGraph
  { cgGraph  :: Graph
  , cgColors :: HM.HashMap Vertex Int
  }
  deriving (Eq, Ord, Show, Read)

parseColored :: Parser ColoredGraph
parseColored = do
  _                  <- many comment
  (n'nodes, n'edges) <- problem
  cs                 <- HM.fromList <$> many (color n'nodes)
  g                  <- buildG (1, n'nodes) <$> count n'edges (edge n'nodes)
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
