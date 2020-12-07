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
    ( Planet(name, climate, diameter, population, terrain) )
import ParsePeople
    ( Person(name, gender, homeworld, height, mass) )
import ParseSpecies ( Species(..) )
import ParseFilms
    ( Film(title, episode_id, opening_crawl, director, producer,
           release_date) )
import Data.Char ( isDigit )

{- | "initialiseDBPlanets" connects to the database and creates a Planets table
    It takes no arguments
    It returns an IO with a HDBC Connection
-}
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

{- | "initialiseDBPeople" connects to the database and creates a People table
    It takes no arguments
    It returns an IO with a HDBC Connection
-}
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

{- | "initialiseDBSpecies" connects to the database and creates a Species table
    It takes no arguments
    It returns an IO with a HDBC Connection
-}
initialiseDBSpecies :: IO Connection
initialiseDBSpecies =
    do
        conn <- connectPostgreSQL "host=localhost dbname=postgres user=postgres password=admin"
        run conn "CREATE TABLE IF NOT EXISTS species (\
            \name VARCHAR(40), \
            \classification VARCHAR(40), \
            \language VARCHAR(40), \
            \homeworld INT REFERENCES planets (planet_id),\
            \species_id SERIAL PRIMARY KEY \
            \)\
            \" []
        commit conn
        return conn

{- | "initialiseDBFilms" connects to the database and creates a Film table
    It takes no arguments
    It returns an IO with a HDBC Connection
-}
initialiseDBFilms :: IO Connection
initialiseDBFilms =
    do
        conn <- connectPostgreSQL "host=localhost dbname=postgres user=postgres password=admin"
        run conn "CREATE TABLE IF NOT EXISTS films (\
            \title VARCHAR(90), \
            \episode_id VARCHAR(40), \
            \opening_crawl TEXT, \
            \director VARCHAR(40), \
            \producer VARCHAR(90), \
            \release_date VARCHAR(40), \
            \film_id SERIAL PRIMARY KEY \
            \)\
            \" []
        commit conn
        return conn

{- | "convertUnkToNothing" converts values of "unknown" to type Nothing
    It should only be used if the JSON value is sometimes "unknown" - do not use for Null values
    It takes one argument: 
    - The String value returned from the API
    It returns either Nothing (if the value is "unknown"), otherwise returns the value itself
-}
convertUnkToNothing :: String -> Maybe String
convertUnkToNothing "unknown" = Nothing
convertUnkToNothing s = Just s

{- | "extractID" gets the ID from any Star Wars API Url
    It takes one argument: 
    - a URL of type Maybe [Char]
    It returns the ID value as type [Char] or Nothing (if Nothing is passed)
-}
extractID :: Maybe [Char] -> Maybe [Char]
extractID url = do
    case url of Nothing -> Nothing
                Just url -> Just [x | x <- url, isDigit x]

{- | "planetToSqlValues" transforms Planet values to SQL
    It takes one argument: 
    - a Record of type Planet
    It returns an array of SQL Values
-}
planetToSqlValues :: Planet -> [SqlValue]
planetToSqlValues planet = [
        toSql $ ParsePlanets.name planet,
        toSql $ convertUnkToNothing $ climate planet,
        toSql $ convertUnkToNothing $ diameter planet,
        toSql $ convertUnkToNothing $ population planet,
        toSql $ convertUnkToNothing $ terrain planet
    ]

{- | "prepareInsertPlanetSmt" prepares a Posgres statement that inserts values into the Planets table
    It takes one argument: 
    - a HDBC Connection
    It returns an IO with the prepared HDBC Statement 
-}
prepareInsertPlanetSmt :: Connection -> IO Statement
prepareInsertPlanetSmt conn = prepare conn "INSERT INTO planets VALUES (?,?,?,?,?)"

{- | "savePlanets" saves values to the People table
    It takes two arguments:
    - an array containing type 'Species'
    - an HDBC Connecetion
    It returns an empty IO
-}
savePlanets :: [Planet] -> Connection -> IO ()
savePlanets planets conn = do
    stmt <- prepareInsertPlanetSmt conn
    executeMany stmt (map planetToSqlValues planets)
    commit conn

{- | "personToSqlValues" transforms Person values to SQL
    It takes one argument: 
    - a Record of type Person
    It returns an array of SQL Values
-}
personToSqlValues :: Person -> [SqlValue]
personToSqlValues person = [
        toSql $ ParsePeople.name person,
        toSql $ convertUnkToNothing $ gender person,
        toSql $ extractID $ Just (ParsePeople.homeworld person),
        toSql $ convertUnkToNothing $ height person,
        toSql $ convertUnkToNothing $ mass person
    ]

{- | "prepareInsertPeopleSmt" prepares a Posgres statement that inserts values into the People table
    It takes one argument: 
    - a HDBC Connection
    It returns an IO with the prepared HDBC Statement 
-}
prepareInsertPeopleSmt :: Connection -> IO Statement
prepareInsertPeopleSmt conn = prepare conn "INSERT INTO people VALUES (?,?,?,?,?)"

{- | "savePeople" saves values to the People table
    It takes two arguments:
    - an array containing type 'Species'
    - an HDBC Connecetion
    It returns an empty IO
-}
savePeople :: [Person] -> Connection -> IO ()
savePeople people conn = do
    stmt <- prepareInsertPeopleSmt conn
    executeMany stmt (map personToSqlValues people)
    commit conn

{- | "speciesToSqlValues" transforms Species values to SQL
    It takes one argument: 
    - a Record of type Species
    It returns an array of SQL Values
-}
speciesToSqlValues :: Species -> [SqlValue]
speciesToSqlValues species = [
        toSql $ ParseSpecies.name species,
        toSql $ convertUnkToNothing $ classification species,
        toSql $ convertUnkToNothing $ language species,
        toSql $ extractID $ ParseSpecies.homeworld species
    ]

{- | "prepareInsertSpeciesSmt" prepares a Posgres statement that inserts values into the Species table
    It takes one argument: 
    - a HDBC Connection
    It returns an IO with the prepared HDBC Statement 
-}
prepareInsertSpeciesSmt :: Connection -> IO Statement
prepareInsertSpeciesSmt conn = prepare conn "INSERT INTO species VALUES (?,?,?,?)"

{- | "saveSpecies" saves values to the Species table
    It takes two arguments:
    - an array containing type 'Species'
    - an HDBC Connecetion
    It returns an empty IO
-}
saveSpecies :: [Species] -> Connection -> IO ()
saveSpecies species conn = do
    stmt <- prepareInsertSpeciesSmt conn
    executeMany stmt (map speciesToSqlValues species)
    commit conn

{- | "filmToSqlValues" transforms Film values to SQL
    It takes one argument: 
    - a Record of type Film
    It returns an array of SQL Values
-}
filmToSqlValues :: Film -> [SqlValue]
filmToSqlValues film = [
        toSql $ title film,
        toSql $ episode_id film,
        toSql $ opening_crawl film,
        toSql $ director film,
        toSql $ producer film,
        toSql $ release_date film
    ]

{- | "prepareInsertFilmSmt" prepares a Posgres statement that inserts values into the Films table
    It takes one argument: 
    - a HDBC Connection
    It returns an IO with the prepared HDBC Statement 
-}
prepareInsertFilmSmt :: Connection -> IO Statement
prepareInsertFilmSmt conn = prepare conn "INSERT INTO films VALUES (?,?,?,?,?,?)"

{- | "saveFilms" saves values to the Films table
    It takes two arguments: 
    - an array of films
    - a HDBC Connection
    It returns an empty IO
-}
saveFilms :: [Film] -> Connection -> IO ()
saveFilms films conn = do
    stmt <- prepareInsertFilmSmt conn
    executeMany stmt (map filmToSqlValues films)
    commit conn