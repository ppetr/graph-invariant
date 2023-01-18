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
{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
module Main
  ( main
  ) where

import           Control.Exception              ( IOException
                                                , evaluate
                                                , handle
                                                )
import           Data.Aeson.Encode.Pretty
import           Data.Attoparsec.Text           ( parseOnly )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import qualified Data.Vector.Generic           as VG
                                                ( map )
import           Data.Word                      ( Word64 )
import qualified System.AtomicWrite.Writer.LazyByteString
                                               as BSL
import qualified System.AtomicWrite.Writer.LazyText
                                               as TL
import           System.Console.CmdArgs  hiding ( name )
import           System.Exit
import           System.IO                      ( hPrint
                                                , hPutStrLn
                                                , stderr
                                                )
import           Text.Printf

import           Data.Graph.Dimacs.Parse
import           Data.Graph.Invariant.Algebra
import           Data.Graph.Invariant.Gap
import           Data.Graph.Invariant.Output
import           Data.Graph.Invariant.Perfect
import           Data.Graph.Invariant.RunStats
import           Data.Graph.Invariant.Types     ( F )

data Args = Args
  { input      :: String
  , output     :: Maybe String
  , gap_output :: Maybe String
  }
  deriving (Show, Data, Typeable)

getRunStats :: IO (Maybe RunStats)
getRunStats =
  handle (\e -> hPrint stderr (e :: IOException) >> return Nothing) $ do
    rt <- currentRunTime
    return . Just $ RunStats
      { runTimeSeconds = Just (runTimeKernel rt + runTimeUser rt)
      }

annotatedArgs :: Args
annotatedArgs =
  Args
      { input      = def &= argPos 0 &= typ "INFILE"
      , output     =
        def
        &= typ "OUTFILE"
        &= help
             "Output file path where to write the detailed results in the JSON format (optional)"
      , gap_output =
        def
        &= typ "OUTFILE"
        &= help
             "Output file path where to write the automorphism group in the GAP format (optional)"
      }
    &= program "graph-invariant"
    &= summary
         "Computes invariants and automorphism groups of graphs. See https://github.com/ppetr/graph-invariant."

main :: IO ()
main = do
  main_args <- cmdArgs annotatedArgs
  t         <- T.readFile (input main_args)
  dGraph    <- case parseOnly parseColored t of
    Left  err     -> hPutStrLn stderr err >> exitWith (ExitFailure 1)
    Right dGraph' -> return dGraph'
  let uGraph      = dGraph { cgGraph = undirected (cgGraph dGraph) }
      (i, is, ps) = canonicalColoring (return $ graphAlgebra uGraph)
  _     <- evaluate i
  stats <- getRunStats

  putStrLn . printf "%#010x" . (fromIntegral :: F -> Word64) $ i

  case gap_output main_args of
    Just f  -> TL.atomicWriteFile f $ automToGap ps
    Nothing -> return ()
  case output main_args of
    Just f -> BSL.atomicWriteFile f . encodePretty $ GraphInvariant
      { name                  = Just (T.pack $ input main_args)
      , invariantVersion      = "TODO"
      , invariant             = fromIntegral i
      , elementInvariants     = fmap (VG.map fromIntegral) is
      , isomorphismGenerators = fmap (VG.map (+ 1)) ps
      , runStats              = stats
      }
    Nothing -> return ()
