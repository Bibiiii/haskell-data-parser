{-# LANGUAGE DeriveGeneric #-}

module Parse
    ( parse,
    AllResults (people, planets, films, species, vehicles, starships)
    ) where

import Data.Aeson ( eitherDecode, FromJSON, ToJSON )
import qualified Data.ByteString.Lazy.Char8 as L8
import GHC.Generics ( Generic )

{- | The "AllResults" constructor defines the types of the root API results
    It is a derivation of the Show and Generic instances
-}
data AllResults = AllResults {
            people :: String,
            planets :: String,
            films :: String,
            species :: String,
            vehicles :: String,
            starships :: String
        } deriving (Show, Generic)

-- | Makes the "AllResults" type an instance of Aeson's FromJSON
instance FromJSON AllResults
-- | Makes the "AllResults" type an instance of Aeson's ToJSON
instance ToJSON AllResults

{- | "parse" decodes the JSON returned by the root API result
    It takes one argument: 
    - a ByteString
    It returns either an error String, or a Record of type AllResults
-}
parse :: L8.ByteString -> Either String AllResults
parse json = eitherDecode json :: Either String AllResults