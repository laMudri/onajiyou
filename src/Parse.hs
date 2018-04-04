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

{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}

module Parse where

import Data.Map.Strict
import Data.Maybe

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Pos

data InputSyntax rs =
    Shapes rs
  | InputSyntax rs :+ InputSyntax rs
  | InputSyntax rs :* InputSyntax rs
  | InputSyntax rs :- InputSyntax rs

metaChars :: [Char]
metaChars = "*＊-−ー()（）"

parens :: (Stream s m Char) => ParsecT s u m a -> ParsecT s u m a
parens p = do
  oneOf "(（"
  res <- p
  oneOf ")）"
  return res

justP :: (Stream s m Char) => (Char -> Maybe a) -> ParsecT s u m a
justP f = tokenPrim (\c -> show [c])
                    (\pos c _cs -> updatePosChar pos c)
                    f

inputSyntaxP :: forall k rs. (Ord k) => (Char -> k) -> Map k rs ->
                Parsec String () (InputSyntax rs)
inputSyntaxP mkK kr = buildExpressionParser table term
  where
  table = [ [Infix diffP AssocLeft]
          , [Infix conjP AssocLeft]
          , [Infix concP AssocLeft]
          ]

  term  =  parens (inputSyntaxP mkK kr)
       <|> (Shapes <$> kanjiP)

  junkP :: Parsec String () ()
  junkP = skipMany (satisfy p) <?> "junk"
    where
    p :: Char -> Bool
    p c = not (c `elem` metaChars) && isNothing (kr !? mkK c)

  kanjiP :: Parsec String () rs
  kanjiP = do
    junkP
    rs <- justP (\ c -> kr !? mkK c)
    junkP
    return rs

  diffP :: Parsec String () (InputSyntax rs -> InputSyntax rs -> InputSyntax rs)
  diffP = (oneOf "-−ー" *> return (:-)) <?> "-"

  conjP :: Parsec String () (InputSyntax rs -> InputSyntax rs -> InputSyntax rs)
  conjP = (oneOf "*＊" *> return (:*)) <?> "*"

  concP :: Parsec String () (InputSyntax rs -> InputSyntax rs -> InputSyntax rs)
  concP = return (:+) <?> "+"
