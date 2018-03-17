{-
This file is part of onajiyou.

onajiyou is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

onajiyou is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with onajiyou.  If not, see <http://www.gnu.org/licenses/>.
-}

module Data where

import Control.Monad
import qualified Data.Foldable as F
import Data.List
import qualified Data.List as List
import Data.Map.Strict --(Map, (!?))
import qualified Data.Map.Strict as Map
import System.Exit
import System.IO

newtype Kanji = MkKanji { kanjiChar :: Char } deriving (Eq, Ord, Show, Read)
newtype Shape = MkShape { shapeChar :: Char } deriving (Eq, Ord, Show, Read)

kanjiShapesList :: IO [(Kanji, [Shape])]
kanjiShapesList = withFile "data/kradfile-u-clean.txt" ReadMode $ \ h -> do
  hSetEncoding h utf8
  fileText <- hGetContents h
  let fileLines = lines fileText
  forM fileLines $ \ l -> do
    case l of
         k : ':' : rs -> return (MkKanji k, List.map MkShape rs)
         _ -> die $ "Malformed line in data file: " ++ l

kanjiShapes :: IO (Map Kanji [Shape])
kanjiShapes = do
  result <- kanjiShapesList
  return $ Map.fromList result

shapeKanjis :: IO (Map Shape [Kanji])
shapeKanjis = do
  list <- kanjiShapesList
  return $ F.foldl'
    (\ acc (k, rs) ->
       unionWith (++)
                 (F.foldl' (\ acc r -> Map.insertWith (++) r [k] acc)
                           Map.empty
                           rs)
                 acc)
    Map.empty
    list
