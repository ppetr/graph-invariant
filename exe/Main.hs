module Main
  ( main
  ) where

import           Data.Attoparsec.Text           ( parseOnly )
import qualified Data.Text.IO                  as T
import           Data.Word                      ( Word64 )
import           System.Environment             ( getArgs )
import           Text.Printf

import           Data.Graph.Dimacs.Parse
import           Data.Graph.Invariant.Matrix
import           Data.Graph.Invariant.Perfect

main :: IO ()
main = do
  [filename] <- getArgs
  t          <- T.readFile filename
  let
    Right dGraph = parseOnly parseColored t
    uGraph       = dGraph { cgGraph = undirected (cgGraph dGraph) }
    f            = invariantMatrixF uGraph
    (i, _) = canonicalColoring (return $ Algebra (cgSize uGraph) (return . f))
  putStrLn . printf "%s,%#010x" filename . (fromIntegral :: F -> Word64) $ i
