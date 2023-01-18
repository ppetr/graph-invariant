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
module Data.Graph.Invariant.Gap
  ( automToGap
  ) where

import           Data.Foldable
import           Data.Text.Lazy                 ( Text )
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text
import qualified Data.Vector.Storable          as VS

listPerm :: VS.Vector Int -> Doc ann
listPerm =
  ("PermList" <>)
    . nest 2
    . parens
    . brackets
    . fillSep
    . punctuate ","
    . map (pretty . (+ 1))
    . VS.toList

automToGap :: (Foldable f) => f (VS.Vector Int) -> Text
automToGap ps = renderLazy . layoutSmart defaultLayoutOptions $ vsep
  [ "# Read this file with ReadAsFunction."
  , "return Group" <> nest
    2
    (  (parens . brackets)
        (line <> (align . vsep . punctuate "," . map listPerm . toList $ ps))
    <> ";"
    )
  ]
