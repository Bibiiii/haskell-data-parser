{-# LANGUAGE DeriveGeneric #-}

-- | Module which helps parse JSON results for the type People
module ParsePeople
    ( parsePeople,
      PeopleResults ( results, next ),
      Person ( name, height, mass, hair_color, skin_color, eye_color, birth_year, gender, homeworld, films, species, vehicles, starships )
    ) where

import Data.Aeson ( eitherDecode, FromJSON, ToJSON )
import qualified Data.ByteString.Lazy.Char8 as L8
import GHC.Generics

-- | The Person constructor defines the types of the Person Record
--   It is a derivation of the Show and Generic instances

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
            films :: [String],
            species :: [Maybe String],
            vehicles :: [Maybe String],
            starships :: [Maybe String],
            created :: String,
            edited :: String,
            url :: String
        } deriving (Show, Generic)

-- | Makes the Person type an instance of Aeson's FromJSON
instance FromJSON Person
-- | Makes the Person type an instance of Aeson's ToJSON
instance ToJSON Person

-- | The PeopleResults constructor defines the types of the People API Results
--   It is a derivation of the Show and Generic instances

data PeopleResults = PeopleResults {
            count :: Int,
            next :: Maybe String,
            previous :: Maybe String,
            results :: [Person]
        } deriving (Show, Generic)

-- | Makes the PeopleResults type an instance of Aeson's FromJSON
instance FromJSON PeopleResults
-- | Makes the PeopleResults type an instance of Aeson's ToJSON
instance ToJSON PeopleResults

-- | Decodes the JSON returned by the people API result
parsePeople :: L8.ByteString -- ^ It takes one argument a ByteString
           -> Either String PeopleResults -- ^ returns either an error String, or a Record of type PeopleResults 
parsePeople json = eitherDecode json :: Either String PeopleResults