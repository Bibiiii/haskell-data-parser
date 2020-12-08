{-# LANGUAGE DeriveGeneric #-}

-- | Module which helps parse JSON results for the type Planets
module ParsePlanets
    ( parsePlanets,
      PlanetResults ( results, next ),
      Planet ( name, rotation_period, orbital_period, diameter, climate, gravity, terrain, surface_water, population )
    ) where

import Data.Aeson ( eitherDecode, FromJSON, ToJSON )
import qualified Data.ByteString.Lazy.Char8 as L8
import GHC.Generics

-- | The Planet constructor defines the types of the Planet Record
--   It is a derivation of the Show and Generic instances

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


-- | Makes the Planet type an instance of Aeson's FromJSON
instance FromJSON Planet
-- | Makes the Planet type an instance of Aeson's ToJSON
instance ToJSON Planet

-- | The PlanetResults constructor defines the types of the Planet API Results
--   It is a derivation of the Show and Generic instances

data PlanetResults = PlanetResults {
            count :: Int,
            next :: Maybe String,
            previous :: Maybe String,
            results :: [Planet]
        } deriving (Show, Generic)

-- | Makes the PlanetResults type an instance of Aeson's FromJSON
instance FromJSON PlanetResults
-- | Makes the PlanetResults type an instance of Aeson's ToJSON
instance ToJSON PlanetResults

-- | Decodes the JSON returned by the Planets API result
parsePlanets :: L8.ByteString -- ^ It takes one argument a ByteString
           -> Either String PlanetResults -- ^ returns either an error String, or a Record of type PlanetResults 
parsePlanets json = eitherDecode json :: Either String PlanetResults