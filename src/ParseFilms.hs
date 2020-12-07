{-# LANGUAGE DeriveGeneric #-}

module ParseFilms
    ( parseFilms,
      FilmResults ( results, next ),
      Film ( episode_id, title, director, producer, species, characters, planets, release_date, starships, vehicles, opening_crawl )
    ) where

import Data.Aeson ( eitherDecode, FromJSON, ToJSON )
import qualified Data.ByteString.Lazy.Char8 as L8
import GHC.Generics

{- | The "Film" constructor defines the types of the Film Record
    It is a derivation of the Show and Generic instances
-}
data Film = Film {
            episode_id :: Int,
            title :: String,
            opening_crawl :: String,
            director :: String,
            producer :: String,
            release_date :: String,
            characters :: [String],
            planets :: [String],
            starships :: [String],
            vehicles :: [String],
            species :: [String]
        } deriving (Show, Generic)

-- | Makes the "Film" type an instance of Aeson's FromJSON
instance FromJSON Film
-- | Makes the "Film" type an instance of Aeson's ToJSON
instance ToJSON Film

{- | The "FilmResults" constructor defines the types of the Film API Results
    It is a derivation of the Show and Generic instances
-}
data FilmResults = FilmResults {
            count :: Int,
            next :: Maybe String,
            previous :: Maybe String,
            results :: [Film]
        } deriving (Show, Generic)

-- | Makes the "FilmResults" type an instance of Aeson's FromJSON
instance FromJSON FilmResults
-- | Makes the "FilmResults" type an instance of Aeson's ToJSON
instance ToJSON FilmResults

{- | "parseFilms" decodes the JSON returned by the films API result
    It takes one argument: 
    - a ByteString
    It returns either an error String, or a Record of type FilmResults
-}  
parseFilms :: L8.ByteString -> Either String FilmResults
parseFilms json = eitherDecode json :: Either String FilmResults