module Main
  ( main
  ) where

import           Data.Attoparsec.Text           ( parseOnly )
import qualified Data.Text.IO                  as T
import           Data.Word                      ( Word64 )
import           System.Environment             ( getArgs )
import           Text.Printf

import           Data.Graph.Dimacs.Parse
import           Data.Graph.Invariant.Algebra
import           Data.Graph.Invariant.Perfect
import           Data.Graph.Invariant.Types

main :: IO ()
main = do
  [filename] <- getArgs
  t          <- T.readFile filename
  let Right dGraph = parseOnly parseColored t
      uGraph       = dGraph { cgGraph = undirected (cgGraph dGraph) }
      (i, _)       = canonicalColoring (return $ graphAlgebra uGraph)
  putStrLn . printf "%s,%#010x" filename . (fromIntegral :: F -> Word64) $ i
