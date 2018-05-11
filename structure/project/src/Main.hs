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

{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main
  (
    main
  )
  where

import           Control.Monad        (join, (>=>))
import           Control.Monad.Except (MonadError, MonadIO, liftEither, liftIO,
                                       runExceptT, throwError)
import           Data.Aeson           (FromJSON, eitherDecode)
import qualified Data.ByteString.Lazy as BS
import           Data.List            (find)
import           Data.Semigroup       ((<>))
import           System.Environment   (getArgs)
import           Types

type Content = BS.ByteString
type AppInput = (Content, Content, Content, Content)
type AppData = ([UglyHistory], [UglyYearTrash], [UglyYearTrash], [UglyYearTrash])
type AppContext m = (MonadIO m, MonadError String m)

main :: IO ()
main = do
  [population, glass, yellow, green] <- join $ traverse BS.readFile <$> getArgs
  result <- runExceptT $ transform (population, glass, yellow, green)
  putStrLn $ case result of
               Right _    -> "Sucessfully transformed input data"
               Left error -> error

transform :: AppContext m
          => AppInput
          -> m ()
transform = decodeAndSanitize >=> pure . combineTrashes >=> writeXML

decodeAndSanitize :: AppContext m
           => AppInput
           -> m AppData
decodeAndSanitize (population, glass, yellow, green) = do
  history <- fmap sanitizeHistory <$> decodeHistory population
  [glassTrash, yellowTrash, greenTrash] <- traverse decodeTrash [glass, yellow, green]
  pure (history, glassTrash, yellowTrash, greenTrash)
  where
    sanitizeHistory x = x { historyDistrictCode = historyDistrictCode x - 100 }
    decodeHistory x = do
      decoded :: [UglyHistory] <- decode x
      pure decoded
    decodeTrash x = do
      decoded :: [UglyYearTrash] <- decode x
      pure decoded
    decode x = do
      decoded :: [UglyDataset a] <- liftDecode x
      pure $ datasetField <$> decoded
        where
          liftDecode = liftEither . eitherDecode

combineTrashes :: AppData -> [District]
combineTrashes (history, glassTrash, yellowTrash, greenTrash) = mkDistrict . combineTrashes <$> history
  where
    findTrash c = find ((==) c . trashDistrictCode)
    findTrashes c = fmap (findTrash c)
    computeTonnage (UglyYearTrash a b c d e f g h i j k l m) =
      a + b + c + d + e + f + g + h + i + j + k + l
    computeOrDefault = maybe 0 computeTonnage
    combineTrashes h = (h, computeOrDefault <$> findTrashes (historyDistrictCode h) [glassTrash, yellowTrash, greenTrash])
    mkDistrict (h, [glassVolume, yellowVolume, greenVolume]) =
      District (historyDistrictCode h) (historyCity h) (historyYear h) (historyPopulation h) (TrashTonnage glassVolume yellowVolume greenVolume)

writeXML :: AppContext m => [District] -> m ()
writeXML x = mapM_ (debug . show) x

debug :: AppContext m => String -> m ()
debug = liftIO . putStrLn
