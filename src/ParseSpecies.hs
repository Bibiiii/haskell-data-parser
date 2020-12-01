{-# LANGUAGE DeriveGeneric #-}

module ParseSpecies
    ( parseSpecies,
      SpeciesResults ( results, next ),
      Species ( name, classification, language, home_world )
    ) where

import Data.Aeson ( eitherDecode, FromJSON, ToJSON )
import qualified Data.ByteString.Lazy.Char8 as L8
import GHC.Generics

type URL = String

data Species = Species {
            name :: String,
            classification :: String,
            language :: String,
            home_world ::  String
        } deriving (Show, Generic)

instance FromJSON Species
instance ToJSON Species

data SpeciesResults = SpeciesResults {
            count :: Int,
            next :: Maybe String,
            previous :: Maybe String,
            results :: [Species]
        } deriving (Show, Generic)

instance FromJSON SpeciesResults
instance ToJSON SpeciesResults
    
parseSpecies :: L8.ByteString -> Either String SpeciesResults
parseSpecies json = eitherDecode json :: Either String SpeciesResults