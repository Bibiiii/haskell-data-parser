{-# LANGUAGE DeriveGeneric #-}

module Parse
    ( parse,
    AllResults (people, planets, films, species, vehicles, starships)
    ) where

import Data.Aeson ( eitherDecode, FromJSON, ToJSON )
import qualified Data.ByteString.Lazy.Char8 as L8
import GHC.Generics

data AllResults = AllResults {
            people :: String,
            planets :: String,
            films :: String,
            species :: String,
            vehicles :: String,
            starships :: String
        } deriving (Show, Generic)

instance FromJSON AllResults
instance ToJSON AllResults
    
parse :: L8.ByteString -> Either String AllResults
parse json = eitherDecode json :: Either String AllResults