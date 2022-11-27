module Main
  ( main
  ) where

import           Data.Attoparsec.Text           ( parseOnly )
import qualified Data.Text.IO                  as T
                                                ( readFile )
import           Numeric                        ( showHex )
import           System.Environment             ( getArgs )

import           Data.Graph.Dimacs.Parse
import           Data.Graph.Invariant.Matrix

main :: IO ()
main = do
  [filename] <- getArgs
  t          <- T.readFile filename
  let Right cg = parseOnly parseColored t
  print cg
  let u = undirected $ cgGraph cg
  print u
  print (incidence' u)
  print (invariantBase cg)
  print (invariantMatrix cg)
  putStrLn (showHex (invariant cg) "")
