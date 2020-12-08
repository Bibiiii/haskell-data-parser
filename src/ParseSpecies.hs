{-# LANGUAGE DeriveGeneric #-}

-- | Module which helps parse JSON results for the type Species
module ParseSpecies
    ( parseSpecies,
      SpeciesResults ( results, next ),
      Species ( name, classification, language, homeworld )
    ) where

import Data.Aeson ( eitherDecode, FromJSON, ToJSON )
import qualified Data.ByteString.Lazy.Char8 as L8
import GHC.Generics

-- | The Species constructor defines the types of the Species Record
--   It is a derivation of the Show and Generic instances

data Species = Species {
            name :: String,
            classification :: String,
            language :: String,
            homeworld ::  Maybe String
        } deriving (Show, Generic)

-- | Makes the Species type an instance of Aeson's FromJSON
instance FromJSON Species
-- | Makes the Species type an instance of Aeson's ToJSON
instance ToJSON Species

-- | The "SpeciesResults" constructor defines the types of the Species API Results
--   It is a derivation of the Show and Generic instances

data SpeciesResults = SpeciesResults {
            count :: Int,
            next :: Maybe String,
            previous :: Maybe String,
            results :: [Species]
        } deriving (Show, Generic)

-- | Makes the SpeciesResults type an instance of Aeson's FromJSON
instance FromJSON SpeciesResults
-- | Makes the SpeciesResults type an instance of Aeson's ToJSON
instance ToJSON SpeciesResults

-- | Decodes the JSON returned by the Species API result
parseSpecies :: L8.ByteString -- ^ It takes one argument a ByteString
           -> Either String SpeciesResults -- ^ returns either an error String, or a Record of type SpeciesResults 
parseSpecies json = eitherDecode json :: Either String SpeciesResults