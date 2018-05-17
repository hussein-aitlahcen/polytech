-- Main.hs ---

-- Copyright (C) 2018 Hussein Ait-Lahcen

-- Author: Hussein Ait-Lahcen <hussein.aitlahcen@gmail.com>

-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License
-- as published by the Free Software Foundation; either version 3
-- of the License, or (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program. If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative      ((<|>))
import           Data.Bifunctor
import           Data.ByteString          as BS
import qualified Data.ByteString.Internal as BS (c2w, w2c)
import           Data.Char
import qualified Data.List                as L
import qualified Data.Set                 as S

data Rule = Rule { conditions  :: S.Set ByteString
                 , conclusions :: S.Set ByteString
                 }
          deriving Show


rawRules :: [ByteString]
rawRules =
  [
    "if boat and sport and sail then sailboat",
    "if boat and pleasure and sail then sailboat",
    "if sailboat and not_triangular_latin_sail then gaff_rig",
    "if keel then keelboat",
    "if not_keel and sailboat then sailing_dinghy and portable",
    "if habitable and sailboat then sailboat_cruise and not_portable",
    "if not_longer_than_8 then not_longer_than_13 and not_longer_than_10",
    "if keelboat then not_portable",
    "if longer_than_13 then longer_than_10",
    "if keelboat and not_keelboat_regatte then keelboat_cruise",
    "if keelboat and sport then keelboat_sport",
    "if longer_than_10 then longer_than_8",
    "if keelboat and not_habitable then keelboat_regatte",
    "if not_portable and not_habitable and sailboat then sailboat_promenade",
    "if keelboat_cruise then sailboat_cruise",
    "if sailboat_cruise and longer_than_8 and not_longer_than_10 then cruise_semi_offshore",
    "if sailboat_cruise and longer_than_10 then racing_can and cruise_offshore",
    "if sailboat_cruise and not_longer_than_8 then sailboat_cruise_coastal",
    "if sailboat_cruise and number_of_shells_larger_than_1 then sailboat_multishell",
    "if longer_than_13 and racing_can then transoceanic_race"
  ]

rawFacts :: [ByteString]
rawFacts =
  [
    "longer_than_13",
    "habitable",
    "not_keel",
    "boat",
    "sport",
    "sail"
  ]

trim :: ByteString -> ByteString
trim =
  let f = BS.reverse . BS.dropWhile (== BS.c2w ' ')
  in f . f

tokenise :: ByteString -> ByteString -> [ByteString]
tokenise x y =
  let (h, t) = breakSubstring x y
  in h : if BS.null t then [] else tokenise x (BS.drop (BS.length x) t)

parseRule :: ByteString -> Rule
parseRule rule =
  let [x, y] = (tokenise "then" . BS.drop 3) rule
      conds  = tokenise "and" x
      concl  = tokenise "and" y
  in Rule (S.fromList $ trim <$> conds) (S.fromList $ trim <$> concl)

forward :: S.Set ByteString -> Int -> Int -> Int -> [Rule] -> (Int, S.Set ByteString)
forward c 0 j k rules = (k, c)
forward c i j k rules =
  let (Rule rcond rconc) = rules !! j
      c' = if L.all (`L.elem` c) rcond then S.union c rconc else c
      i' = i - 1 + (L.length c' - L.length c)
      j' = (j + 1) `mod` L.length rules
      k' = k + 1
  in forward c' i' j' k' rules

main :: IO ()
main =
  let rules = parseRule <$> rawRules
  in print $ second (\x -> (L.length x, x)) $ forward (S.fromList rawFacts) (L.length rules) 0 0 rules
