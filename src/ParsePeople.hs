{-# LANGUAGE DeriveGeneric #-}

module ParsePeople
    ( parsePeople,
      PeopleResults ( results, next ),
      Person ( name, height, mass, hair_color, skin_color, eye_color, birth_year, gender, homeworld, films, species, vehicles, starships )
    ) where

import Data.Aeson ( eitherDecode, FromJSON, ToJSON )
import qualified Data.ByteString.Lazy.Char8 as L8
import GHC.Generics

type URL = String

data Person = Person {
            name :: String,
            height :: String,
            mass :: String,
            hair_color :: String,
            skin_color :: String,
            eye_color :: String,
            birth_year :: String,
            gender :: String,
            homeworld ::  String,
            films :: [URL],
            species :: [Maybe URL],
            vehicles :: [Maybe URL],
            starships :: [Maybe URL],
            created :: String,
            edited :: String,
            url :: URL
        } deriving (Show, Generic)

instance FromJSON Person
instance ToJSON Person

data PeopleResults = PeopleResults {
            count :: Int,
            next :: Maybe String,
            previous :: Maybe String,
            results :: [Person]
        } deriving (Show, Generic)

instance FromJSON PeopleResults
instance ToJSON PeopleResults
    
parsePeople :: L8.ByteString -> Either String PeopleResults
parsePeople json = eitherDecode json :: Either String PeopleResults