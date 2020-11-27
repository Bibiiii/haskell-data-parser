{-# LANGUAGE DeriveGeneric #-}

module ParsePlanets
    ( parsePlanets,
      PlanetResults ( results, next ),
      Planet ( name, rotation_period, orbital_period, diameter, climate, gravity, terrain, surface_water, population )
    ) where

import Data.Aeson ( eitherDecode, FromJSON, ToJSON )
import qualified Data.ByteString.Lazy.Char8 as L8
import GHC.Generics

data Planet = Planet {
            name :: String,
            rotation_period :: String,
            orbital_period :: String,
            diameter :: String,
            climate :: String,
            gravity :: String,
            terrain :: String,
            surface_water :: String,
            population ::  String
        } deriving (Show, Generic)

instance FromJSON Planet
instance ToJSON Planet

data PlanetResults = PlanetResults {
            count :: Int,
            next :: Maybe String,
            previous :: Maybe String,
            results :: [Planet]
        } deriving (Show, Generic)

instance FromJSON PlanetResults
instance ToJSON PlanetResults
    
parsePlanets :: L8.ByteString -> Either String PlanetResults
parsePlanets json = eitherDecode json :: Either String PlanetResults