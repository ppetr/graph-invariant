{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  ) where

import           Data.Aeson.Encode.Pretty
import           Data.Attoparsec.Text           ( parseOnly )
import qualified Data.ByteString.Lazy.Char8    as BS
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import qualified Data.Vector.Generic           as VG
                                                ( map )
import           System.Environment             ( getArgs )

import           Data.Graph.Dimacs.Parse
import           Data.Graph.Invariant.Algebra
import           Data.Graph.Invariant.Output
import           Data.Graph.Invariant.Perfect

main :: IO ()
main = do
  [filename] <- getArgs
  t          <- T.readFile filename
  let Right dGraph = parseOnly parseColored t
      uGraph       = dGraph { cgGraph = undirected (cgGraph dGraph) }
      (i, is, ps)  = canonicalColoring (return $ graphAlgebra uGraph)
  BS.putStrLn . encodePretty $ GraphInvariant
    { name                  = Just (T.pack filename)
    , invariantVersion      = "TODO"
    , invariant             = fromIntegral i
    , elementInvariants     = fmap (VG.map fromIntegral) is
    , isomorphismGenerators = ps
    }
