module ProcessJson
    (
        processJson,
    ) where

import HTTP(download)
import qualified Data.ByteString.Lazy.Char8 as L8
import ParsePlanets
import ParsePeople
import ParseFilms
import ParseSpecies
import Database
import Parse

savePlanetJson :: PlanetResults -> IO ()
savePlanetJson res = do
    conn <- initialiseDBPlanets
    case ParsePlanets.next res of
        Just a -> do
            savePlanets (ParsePlanets.results res) conn
            processJson a "planets"
        Nothing -> savePlanets (ParsePlanets.results res) conn

savePeopleJson :: PeopleResults -> IO ()
savePeopleJson res = do
    conn <- initialiseDBPeople
    case ParsePeople.next res of
        Just a -> do
            savePeople (ParsePeople.results res) conn
            processJson a "people"
        Nothing -> savePeople (ParsePeople.results res) conn

saveFilmsJson :: FilmResults -> IO ()
saveFilmsJson res = do
    conn <- initialiseDBFilms
    case ParseFilms.next res of
        Just a -> do
            saveFilms (ParseFilms.results res) conn
            processJson a "films"
        Nothing -> saveFilms (ParseFilms.results res) conn


saveSpeciesJson :: SpeciesResults -> IO ()
saveSpeciesJson res = do
    conn <- initialiseDBSpecies
    case ParseSpecies.next res of
        Just a -> do
            saveSpecies (ParseSpecies.results res) conn
            processJson a "species"
        Nothing -> saveSpecies (ParseSpecies.results res) conn
                
-- saveJson f res str = do
--     conn <- initialiseDB
--     case next res of
--         Just a -> do
--             f (results res) conn
--             processJson a str
--         Nothing -> f (results res) conn

catchErr :: Show a => (t1 -> Either a t2) -> t1 -> (t2 -> IO ()) -> IO ()
catchErr f json g = do
    case f json of
        Left err -> do
            print err
        Right res -> do
            g res

parseDirectories :: L8.ByteString -> IO ()
parseDirectories json = do
    -- catchErr parse json processJson
    case parse json of
            Left err -> do
                print err
            Right res -> do
                processJson (Parse.planets res) "planets"
                processJson (people res) "people"
                processJson (Parse.species res) "species"
                processJson (Parse.films res) "films"


processJson :: String -> String -> IO ()
processJson url dir = do
    print $ "Downloading and parsing url: " ++ url
    json <- download url
    case dir of
        "root" -> do
            print "Parsing API root"
            parseDirectories json
        "planets" -> do
            print "Parsing planets"
            catchErr parsePlanets json savePlanetJson
        "people" -> do
            print "Parsing people"
            catchErr parsePeople json savePeopleJson
        "species" -> do
            print "Parsing species"
            catchErr parseSpecies json saveSpeciesJson
        "films" -> do
            print "Parsing films"
            catchErr parseFilms json saveFilmsJson