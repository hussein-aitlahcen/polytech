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

{-# LANGUAGE LambdaCase #-}

module Main where

import           Control.Monad         (join, (>=>))
import           Data.Aeson            (eitherDecodeStrict, encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy  as BSL
import           Data.Semigroup        ((<>))
import           System.Directory.Tree (AnchoredDirTree (..), DirTree (..),
                                        readDirectoryWith)
import           Test.RandomStrings    (onlyAlphaNum, randomChar, randomWord)
import           Text.Read             (readMaybe)
import           Types

database :: String
database = "./db"

fileNameLength :: Int
fileNameLength = 10

write :: String -> IO String
write s = putStrLn s >> pure s

writeCurriculum :: Curriculum -> IO ()
writeCurriculum c = join $ BS.writeFile <$> (prependDirectory . appendExtension <$> generateName) <*> pure (BSL.toStrict $ encode c)
  where
    generateName = randomWord (onlyAlphaNum randomChar) fileNameLength
    prependDirectory = ((database <> "/") <>)
    appendExtension = (<> ".json")

readCurriculum :: BS.ByteString -> Either String Curriculum
readCurriculum = eitherDecodeStrict

readCurriculums :: IO (AnchoredDirTree BS.ByteString)
readCurriculums = readDirectoryWith (write >=> BS.readFile) database

main :: IO ()
main = fmap (traverse readCurriculum . dirTree) readCurriculums >>= \case
  Right curriculums -> create curriculums
  Left message      -> putStrLn $ "Erreur lors de la lecture d'un curriculum: " <> message

askInput :: String -> IO String
askInput m = putStrLn (m <> ": ") >> getLine

askBoundedInput :: Int -> Int -> String -> IO Int
askBoundedInput l h m =
  readMaybe <$> askInput (m <> "(" <> show l <> "-" <> show h <> ")") >>= \case
    Just input ->
      if input < l || input >= h then
        putStrLn ("Entrée invalide: min=" <> show l <> " max=" <> show h) >> askBoundedInput l h m
      else
        pure input
    Nothing ->
      putStrLn "Retournez faire du PHP." >> askBoundedInput l h m

askTechnologies :: IO ([] Technology)
askTechnologies = askInput "Souhaitez-vous ajouter une compétence (O/N) ?" >>= \case
  "O" -> pure Technology <*> askInput "Technologie" <*> askBoundedInput 1 5 "Niveau"
        >>= \technology -> fmap (technology :) askTechnologies
  "N" -> pure []
  _   -> askTechnologies

create :: DirTree Curriculum -> IO ()
create curriculums = do
  putStrLn "Liste des curriculums enregistrés: "
  mapM_ print curriculums
  putStrLn "Vous êtes sur le point de créer un nouveau CV, veuillez saisir les informations demandées:"
  writeCurriculum =<< pure Curriculum <*> askInput "Nom"
                                    <*> askInput "Prénom"
                                    <*> askInput "Adresse"
                                    <*> askInput "Ville"
                                    <*> askTechnologies
