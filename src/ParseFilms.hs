{-# LANGUAGE DeriveGeneric #-}

module ParseFilms
    ( parseFilms,
      FilmResults ( results, next ),
      Films ( episode_id, title, director, producer, species, characters, planets, release_date, starships, vehicles, opening_crawl )
    ) where

import Data.Aeson ( eitherDecode, FromJSON, ToJSON )
import qualified Data.ByteString.Lazy.Char8 as L8
import GHC.Generics

data Films = Films {
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