module Database
    ( initialiseDBPlanets,
      initialiseDBPeople,
      initialiseDBSpecies, 
      initialiseDBFilms, 
      savePlanets,
      savePeople,
      saveSpecies,
      saveFilms
    ) where

import Database.HDBC
    ( toSql,
      SqlValue,
      Statement(executeMany),
      IConnection(commit, run, prepare) )
import Database.HDBC.PostgreSQL ( Connection, connectPostgreSQL )
import ParsePlanets
import ParsePeople
import ParseSpecies
import ParseFilms
import Data.Char

-- Create table for planets
initialiseDBPlanets :: IO Connection
initialiseDBPlanets =
    do
        conn <- connectPostgreSQL "host=localhost dbname=postgres user=postgres password=admin"
        run conn "CREATE TABLE IF NOT EXISTS planets (\
            \name VARCHAR(40), \
            \climate VARCHAR(40), \
            \diameter INT, \
            \population BIGINT, \
            \terrain VARCHAR(40), \
            \planet_id SERIAL PRIMARY KEY \
            \)\
            \" []
        commit conn
        return conn

-- Create database for people
initialiseDBPeople :: IO Connection
initialiseDBPeople =
    do
        conn <- connectPostgreSQL "host=localhost dbname=postgres user=postgres password=admin"
        run conn "CREATE TABLE IF NOT EXISTS people (\
            \name VARCHAR(40), \
            \gender VARCHAR(40), \
            \homeworld INT REFERENCES planets (planet_id), \
            \height VARCHAR(40), \
            \mass VARCHAR(40), \
            \person_id SERIAL PRIMARY KEY \
            \)\
            \" []
        commit conn
        return conn

-- Create database for species
initialiseDBSpecies :: IO Connection
initialiseDBSpecies =
    do
        conn <- connectPostgreSQL "host=localhost dbname=postgres user=postgres password=admin"
        run conn "CREATE TABLE IF NOT EXISTS species (\
            \name VARCHAR(40) PRIMARY KEY, \
            \classification VARCHAR(40), \
            \language VARCHAR(40), \
            \homeworld INT REFERENCES planets (planet_id)\
            \)\
            \" []
        commit conn
        return conn

-- Create database for films
initialiseDBFilms :: IO Connection
initialiseDBFilms =
    do
        conn <- connectPostgreSQL "host=localhost dbname=postgres user=postgres password=admin"
        run conn "CREATE TABLE IF NOT EXISTS films (\
            \episode_ID VARCHAR(40) PRIMARY KEY, \
            \title VARCHAR(40), \
            \director VARCHAR(40), \
            \producer VARCHAR(40), \
            \species_names VARCHAR(40) REFERENCES species (name),\
            \characters VARCHAR(40) REFERENCES people (name), \
            \planets_names VARCHAR(40) REFERENCES planets (name), \
            \release_dates VARCHAR(40), \
            \)\
            \" []
        commit conn
        return conn

-- convert any 'unknown' values to Nothing
convertUnkToNothing :: String -> Maybe String
convertUnkToNothing "unknown" = Nothing
convertUnkToNothing s = Just s

-- get the homeworld ID from the url
extractHomeworld :: String -> [Char]
extractHomeworld [] = ['0']
extractHomeworld url = [x | x <- url, isDigit x]

-- transform planet values to SQL
planetToSqlValues :: Planet -> [SqlValue]
planetToSqlValues planet = [
        toSql $ ParsePlanets.name planet,
        toSql $ convertUnkToNothing $ climate planet,
        toSql $ convertUnkToNothing $ diameter planet,
        toSql $ convertUnkToNothing $ population planet,
        toSql $ convertUnkToNothing $ terrain planet
    ]

prepareInsertPlanetSmt :: Connection -> IO Statement
prepareInsertPlanetSmt conn = prepare conn "INSERT INTO planets VALUES (?,?,?,?,?)"

-- save planets to DB
savePlanets :: [Planet] -> Connection -> IO ()
savePlanets planets conn = do
    stmt <- prepareInsertPlanetSmt conn
    executeMany stmt (map planetToSqlValues planets)
    commit conn

personToSqlValues :: Person -> [SqlValue]
personToSqlValues person = [
        toSql $ ParsePeople.name person,
        toSql $ convertUnkToNothing $ gender person,
        toSql $ extractHomeworld $ ParsePeople.homeworld person,
        toSql $ convertUnkToNothing $ height person,
        toSql $ convertUnkToNothing $ mass person
    ]

prepareInsertPeopleSmt :: Connection -> IO Statement
prepareInsertPeopleSmt conn = prepare conn "INSERT INTO people VALUES (?,?,?,?,?)"

savePeople :: [Person] -> Connection -> IO ()
savePeople people conn = do
    stmt <- prepareInsertPeopleSmt conn
    executeMany stmt (map personToSqlValues people)
    commit conn

-- transform species values to SQL
speciesToSqlValues :: Species -> [SqlValue]
speciesToSqlValues species = [
        toSql $ ParseSpecies.name species,
        toSql $ convertUnkToNothing $ classification species,
        toSql $ convertUnkToNothing $ language species,
        toSql $ convertUnkToNothing $ ParseSpecies.homeworld species
    ]

prepareInsertSpeciesSmt :: Connection -> IO Statement
prepareInsertSpeciesSmt conn = prepare conn "INSERT INTO species VALUES (?,?,?,?)"

-- save species to DB
saveSpecies :: [Species] -> Connection -> IO ()
saveSpecies species conn = do
    stmt <- prepareInsertSpeciesSmt conn
    executeMany stmt (map speciesToSqlValues species)
    commit conn

-- transform species values to SQL
filmsToSqlValues :: Films -> [SqlValue]
filmsToSqlValues films = [
        toSql $ ParseFilms.episode_ID films,
        toSql $ convertUnkToNothing $ title films,
        toSql $ convertUnkToNothing $ director films,
        toSql $ convertUnkToNothing $ producer films,
        toSql $ convertUnkToNothing $ species_name films,
        toSql $ convertUnkToNothing $ characters films,
        toSql $ convertUnkToNothing $ planets_names films,
        toSql $ convertUnkToNothing $ release_date films
    ]

prepareInsertFilmsSmt :: Connection -> IO Statement
prepareInsertFilmsSmt conn = prepare conn "INSERT INTO films VALUES (?,?,?,?,?,?,?,?)"

-- save species to DB
saveFilms :: [Films] -> Connection -> IO ()
saveFilms films conn = do
    stmt <- prepareInsertFilmsSmt conn
    executeMany stmt (map filmsToSqlValues films)
    commit conn