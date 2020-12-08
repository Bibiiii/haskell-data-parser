
-- | Module which helps process JSON results
module ProcessJson
    (
        processJson,
        savePlanetJson,
        savePeopleJson,
        saveFilmsJson,
        saveSpeciesJson,
        catchErr,
        parseDirectories
    ) where

import HTTP(download)
import qualified Data.ByteString.Lazy.Char8 as L8
import ParsePlanets
import ParsePeople
import ParseFilms
import ParseSpecies
import Database
import Parse

-- | Saves Planet JSON results to the database
savePlanetJson :: PlanetResults -- ^  takes one argument the result from the Planets API directory
                  -> IO () -- ^ returns an Empty IO
savePlanetJson res = do
    conn <- initialiseDBPlanets
    case ParsePlanets.next res of
        Just a -> do
            savePlanets (ParsePlanets.results res) conn
            processJson a "planets"
        Nothing -> savePlanets (ParsePlanets.results res) conn

-- | Saves People JSON results to the database
savePeopleJson :: PeopleResults -- ^  takes one argument the result from the People API directory
                  -> IO () -- ^ returns an Empty IO
savePeopleJson res = do
    conn <- initialiseDBPeople
    case ParsePeople.next res of
        Just a -> do
            savePeople (ParsePeople.results res) conn
            processJson a "people"
        Nothing -> savePeople (ParsePeople.results res) conn

-- | Saves Film JSON results to the database
saveFilmsJson :: FilmResults -- ^  takes one argument the result from the Film API directory
                  -> IO () -- ^ returns an Empty IO
saveFilmsJson res = do
    conn <- initialiseDBFilms
    case ParseFilms.next res of
        Just a -> do
            saveFilms (ParseFilms.results res) conn
            processJson a "films"
        Nothing -> saveFilms (ParseFilms.results res) conn

-- | Saves Species JSON results to the database
saveSpeciesJson :: SpeciesResults -- ^  takes one argument the result from the Species API directory
                  -> IO () -- ^ returns an Empty IO
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


-- |  Catches any error while parsing a JSON
--    If an error occurs, the error is printed. Otherwise the result is returned
--
--    It takes three arguments: 
--
--    - the parsing function to perform on a given JSON
--
--    - a JSON
--
--    - the save function to perform if the parse is successful
--
--    It returns an empty IO

catchErr :: Show a => (t1 -> Either a t2) -> t1 -> (t2 -> IO ()) -> IO ()
catchErr parser json save = do
    case parser json of
        Left err -> do
            print err
        Right res -> do
            save res


-- | Parse Directories is performed on the API's root result.
--
--    It processes the JSONs for each directory within the root (planets, people, species, films)
parseDirectories :: L8.ByteString -- ^ takes one argument the returned JSON of the root directory
                 -> IO () -- ^ returns an empty IO
parseDirectories json = do
    -- catchErr parse json processJson
    case parse json of
            Left err -> do
                print err
            Right res -> do
                processJson (Parse.planets res) "planets"
                processJson (Parse.species res) "species"
                processJson (people res) "people"
                processJson (Parse.films res) "films"

-- | Processes the API result for a given URL and directory
--   This function takes two arguments
  
processJson :: String -- ^ Takes the URL to process
            -> String -- ^ Takes the directory of the API e.g. "planets"
            -> IO () -- ^ returns an empty IO
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
        "species" -> do
            print "Parsing species"
            catchErr parseSpecies json saveSpeciesJson
        "people" -> do
            print "Parsing people"
            catchErr parsePeople json savePeopleJson
        "films" -> do
            print "Parsing films"
            catchErr parseFilms json saveFilmsJson