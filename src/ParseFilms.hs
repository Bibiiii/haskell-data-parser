{-# LANGUAGE DeriveGeneric #-}

module ParseFilms
    ( parseFilms,
      FilmResults ( results, next ),
      Films ( episode_ID, title, director, producer, species_name, characters, planets_names, release_date )
    ) where

import Data.Aeson ( eitherDecode, FromJSON, ToJSON )
import qualified Data.ByteString.Lazy.Char8 as L8
import GHC.Generics

data Films = Films {
            episode_ID :: String,
            title :: String,
            director :: String,
            producer :: String,
            species_name :: String,
            characters :: String,
            planets_names :: String,
            release_date :: String
        } deriving (Show, Generic)

instance FromJSON Films
instance ToJSON Films

data FilmResults = FilmResults {
            count :: Int,
            next :: Maybe String,
            previous :: Maybe String,
            results :: [Films]
        } deriving (Show, Generic)

instance FromJSON FilmResults
instance ToJSON FilmResults
    
parseFilms :: L8.ByteString -> Either String FilmResults
parseFilms json = eitherDecode json :: Either String FilmResults