{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  ) where

import           Control.Exception              ( evaluate )
import           Data.Aeson.Encode.Pretty
import           Data.Attoparsec.Text           ( parseOnly )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import qualified Data.Vector.Generic           as VG
                                                ( map )
import           Data.Word                      ( Word64 )
import           System.AtomicWrite.Writer.LazyByteString
import           System.Environment             ( getArgs )
import           System.Exit
import           System.IO                      ( hPutStrLn
                                                , stderr
                                                )
import           Text.Printf

import           Data.Graph.Dimacs.Parse
import           Data.Graph.Invariant.Algebra
import           Data.Graph.Invariant.Output
import           Data.Graph.Invariant.Perfect
import           Data.Graph.Invariant.Types     ( F )

main :: IO ()
main = do
  [in_file, out_file] <- getArgs
  t                   <- T.readFile in_file
  dGraph              <- case parseOnly parseColored t of
    Left  err     -> hPutStrLn stderr err >> exitWith (ExitFailure 1)
    Right dGraph' -> return dGraph'
  let uGraph      = dGraph { cgGraph = undirected (cgGraph dGraph) }
      (i, is, ps) = canonicalColoring (return $ graphAlgebra uGraph)
  _ <- evaluate i
  putStrLn . printf "%s,%#010x" in_file . (fromIntegral :: F -> Word64) $ i
  atomicWriteFile out_file . encodePretty $ GraphInvariant
    { name                  = Just (T.pack in_file)
    , invariantVersion      = "TODO"
    , invariant             = fromIntegral i
    , elementInvariants     = fmap (VG.map fromIntegral) is
    , isomorphismGenerators = ps
    }
