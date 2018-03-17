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

module Main where

import Data

import Brick
import Brick.Widgets.Edit

import qualified Data.Foldable as F
import Data.Function
import Data.List
import qualified Data.List as L
import Data.Map.Strict

mainWithData :: Map Kanji [Shape] -> Map Shape [Kanji] -> IO ()
mainWithData kr rk = do
  input <- getLine
  let scores = F.foldl' f empty input
  let resultList = fmap fst $ sortBy (flip compare `on` snd) $ assocs scores
  putStrLn $ L.filter (not . (`elem` input)) $ fmap kanjiChar resultList
  where
  f :: Map Kanji Double -> Char -> Map Kanji Double
  f acc c =
    case kr !? MkKanji c of
         Just rs -> unionWith (+) (F.foldl' g empty rs) acc
         Nothing -> acc

  g :: Map Kanji Double -> Shape -> Map Kanji Double
  g acc r = unionWith (+) (F.foldl' (h x) empty ks) acc
    where
    ks = rk ! r
    x = 1 / fromIntegral (length ks)

  h :: Double -> Map Kanji Double -> Kanji -> Map Kanji Double
  h x acc k = insertWith (+) k x acc

data Name = QueryBox
          | Results

data St = St { focusRing :: FocusRing Name
             , queryBox :: Editor String Name
             , results :: Widget Name
             }

uiWithData :: Map Kanji [Shape] -> Map Shape [Kanji] -> IO ()
uiWithData kr rk = do
  let app :: App St e Name
      app = App { appDraw = ?
                , appChooseCursor = ?
                , appHandleEvent = ?
                , appStartEvent = ?
                , appAttrMap = ?
                }
      initialState = St
        { focusRing = focusRing [QueryBox, Results]
        , queryBox = editor QueryBox Nothing ""
        , results = strWrapWith (WrapSettings { preserveIndentation = False
                                              , breakLongWords = True
                                              })
                                ""
        }
  finalState <- defaultMain app initialState

main :: IO ()
main = do
  kr <- kanjiShapes
  rk <- shapeKanjis
  mainWithData kr rk
