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
    Technology (..),
    Curriculum (..)
  )
  where

import           Data.Aeson (FromJSON (..), ToJSON (..), object, withObject,
                             (.:), (.=))

type FirstName = String
type LastName  = String
type Address   = String
type City      = String
type TechLevel = Int

data Technology = Technology { techName  :: String,
                               techLevel :: TechLevel
                             } deriving Show

data Curriculum = Curriculum { firstName    :: FirstName,
                               lastName     :: LastName,
                               address      :: Address,
                               city         :: City,
                               technologies :: [Technology]
                             } deriving Show

instance ToJSON Technology where
  toJSON s = object ["name"  .= techName s,
                     "level" .= techLevel s]

instance FromJSON Technology where
  parseJSON = withObject "technology" $ \o ->
    pure Technology <*> o .: "name"
                    <*> o .: "level"

instance ToJSON Curriculum where
  toJSON c = object ["first_name"   .= firstName c,
                     "last_name"    .= lastName c,
                     "address"      .= address c,
                     "city"         .= city c,
                     "technologies" .= technologies c]

instance FromJSON Curriculum where
  parseJSON = withObject "cirriculum" $ \o ->
    pure Curriculum <*> o .: "first_name"
                    <*> o .: "last_name"
                    <*> o .: "address"
                    <*> o .: "city"
                    <*> o .: "technologies"
