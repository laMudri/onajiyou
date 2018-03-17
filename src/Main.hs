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

import qualified Data.Foldable as F
import Data.Function
import Data.List
import qualified Data.List as L
import Data.Map.Strict
import Data.Maybe as M

import Control.Monad
import Control.Monad.Loops

import System.IO

-- A monoidal fold in a basically meaningless order.
-- O(log n) uses of the binary operator i.
dealingFold :: b -> (a -> b) -> (b -> b -> b) -> [a] -> b
dealingFold n s i = go
  where
  go [] = n
  go (x : []) = s x
  go xs = i (go ys) (go zs)
    where
    (ys , zs) = split xs

    split :: [a] -> ([a] , [a])
    split [] = ([] , [])
    split (x : xs) = let (ys , zs) = split xs in (x : zs , ys)

getInput :: IO (Maybe String)
getInput = do
  putStr "検索字："
  hFlush stdout
  l <- getLine
  return $ case l of { [] -> Nothing ; _ -> Just l }

mainWithData :: Map Kanji [Shape] -> Map Shape [Kanji] -> IO ()
mainWithData kr rk = do
  whileJust_ getInput $ \ input -> do
    let shapes = F.concat (M.mapMaybe (\ c -> kr !? MkKanji c) input)
    let scores = dealingFold empty s (unionWith (+)) shapes
    let resultList = fmap fst $ sortBy (flip compare `on` snd) $ assocs scores
    putStrLn $ L.filter (not . (`elem` input)) $ fmap kanjiChar resultList
  where
  s :: Shape -> Map Kanji Double
  s r = dealingFold empty (\ k -> insertWith (+) k x empty) (unionWith (+)) ks
    where
    ks = rk ! r
    x = 1 / fromIntegral (length ks)

main :: IO ()
main = do
  kr <- kanjiShapes
  rk <- shapeKanjis
  mainWithData kr rk
