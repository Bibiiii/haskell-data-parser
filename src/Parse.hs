{-# LANGUAGE DeriveGeneric #-}
-- | Module which helps parse JSON results
module Parse
    ( parse,
    AllResults (people, planets, films, species, vehicles, starships)
    ) where

import Data.Aeson ( eitherDecode, FromJSON, ToJSON )
import qualified Data.ByteString.Lazy.Char8 as L8
import GHC.Generics ( Generic )

-- | The AllResults constructor defines the types of the root API results
--
--    It is a derivation of the Show and Generic instances
data AllResults = AllResults {
            people :: String,
            planets :: String,
            films :: String,
            species :: String,
            vehicles :: String,
            starships :: String
        } deriving (Show, Generic)

-- | Makes the AllResults type an instance of Aeson's FromJSON
instance FromJSON AllResults
-- | Makes the AllResults type an instance of Aeson's ToJSON
instance ToJSON AllResults

-- | Decodes the JSON returned by the root API result
parse :: L8.ByteString -- ^ It takes one argument a ByteString 
      -> Either String AllResults -- ^ returns either an error String, or a Record of type AllResults
parse json = eitherDecode json :: Either String AllResults