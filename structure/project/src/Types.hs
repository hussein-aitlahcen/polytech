-- Types.hs ---

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

module Types
  (
    UglyDataset (..),
    UglyYearTrash (..),
    UglyHistory (..),
    TrashTonnage (..),
    District (..)
  )
  where

import           Data.Aeson (FromJSON (..), ToJSON (..), object, withObject,
                             (.:), (.=))

data UglyHistory = UglyHistory { historyYear         :: Int
                               , historyPopulation   :: Int
                               , historyDistrictCode :: Int
                               , historyCity         :: String
                               }

data UglyYearTrash = UglyYearTrash { trashJanuary      :: Float
                                   , trashFebruary     :: Float
                                   , trashMarch        :: Float
                                   , trashApril        :: Float
                                   , trashMay          :: Float
                                   , trashJune         :: Float
                                   , trashJuly         :: Float
                                   , trashAugust       :: Float
                                   , trashSeptember    :: Float
                                   , trashOctober      :: Float
                                   , trashNovember     :: Float
                                   , trashDecember     :: Float
                                   , trashDistrictCode :: Int
                                   }

data UglyDataset a =  UglyDataset { datasetId       :: String
                                  , recordId        :: String
                                  , datasetField    :: a
                                  , recordTimestamp :: String
                                  }

data TrashTonnage = TrashTonnage { tonnageGlass  :: Float
                                 , tonnageGreen  :: Float
                                 , tonnageYellow :: Float
                                 } deriving Show

data District = District { districtCode       :: Int
                         , districtCity       :: String
                         , districtYear       :: Int
                         , districtPopulation :: Int
                         , districtTrash      :: TrashTonnage
                         } deriving Show

instance FromJSON UglyHistory where
  parseJSON = withObject "history" $ \v ->
    pure UglyHistory
    <*> (read <$> v .: "annee")
    <*> v .: "population_municipale"
    <*> (read <$> v .: "code_insee")
    <*> v .: "nom_commune"

instance FromJSON UglyYearTrash where
  parseJSON = withObject "trash" $ \v ->
    pure UglyYearTrash
    <*> v .: "janv_11"
    <*> v .: "fevr_11"
    <*> v .: "mars_11"
    <*> v .: "avr_11"
    <*> v .: "mai_11"
    <*> v .: "juin_11"
    <*> v .: "juil_11"
    <*> v .: "aout_11"
    <*> v .: "sept_11"
    <*> v .: "oct_11"
    <*> v .: "nov_11"
    <*> v .: "dec_11"
    <*> v .: "granularite"

instance FromJSON a => FromJSON (UglyDataset a) where
  parseJSON = withObject "dataset" $ \v ->
    pure UglyDataset
    <*> v .: "datasetid"
    <*> v .: "recordid"
    <*> v .: "fields"
    <*> v .: "record_timestamp"

instance ToJSON TrashTonnage where
  toJSON x = object ["verre" .= tonnageGlass  x,
                     "vert"  .= tonnageGreen  x,
                     "jaune" .= tonnageYellow x]

instance ToJSON District where
  toJSON x = object ["code"       .= districtCode       x,
                     "commune"    .= districtCity       x,
                     "annee"      .= districtYear       x,
                     "population" .= districtPopulation x,
                     "dechets"    .= districtTrash      x]
