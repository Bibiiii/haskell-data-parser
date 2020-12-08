-- | Module to Manage the Creation of the database and any operations performed on the database.
module Database
    ( initialiseDBPlanets,
      initialiseDBPeople,
      initialiseDBSpecies, 
      initialiseDBFilms, 
      savePlanets,
      savePeople,
      saveSpecies,
      saveFilms,
      convertUnkToNothing,
      extractID,
      planetToSqlValues,
      prepareInsertPlanetSmt,
      personToSqlValues,
      prepareInsertPeopleSmt,
      speciesToSqlValues,
      prepareInsertSpeciesSmt,
      filmToSqlValues,
      prepareInsertFilmSmt,      
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
    ( Person(name, gender, homeworld, height, mass, species) )
import ParseSpecies ( Species(..) )
import ParseFilms
    ( Film(title, episode_id, opening_crawl, director, producer,
           release_date) )
import Data.Char ( isDigit )

-- DATABASE SETUP

-- | Connects to the database and creates a Planets table
--
--    This function takes no arguments
--
--    This function returns an IO with a HDBC Connection

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

-- | Connects to the database and creates a Species table
--
--    This function takes no arguments
--
--    This function returns an IO with a HDBC Connection
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

-- | Connects to the database and creates a People table
--
--    This function takes no arguments
--
--    This function returns an IO with a HDBC Connection
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
            \species INT REFERENCES species (species_id), \
            \person_id SERIAL PRIMARY KEY \
            \)\
            \" []
        commit conn
        return conn

-- | Connects to the database and creates a Films table
--
--    This function takes no arguments
--
--    This function returns an IO with a HDBC Connection
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

-- GENERAL

-- |Converts values of "unknown" to type Nothing
--
--  It should only be used if the JSON value is sometimes "unknown" - do not use for Null values
--
--  It takes a single argument
--
convertUnkToNothing :: String -- ^  Takes the String value returned from the API
                    -> Maybe String -- ^ Returns either Nothing (if the value is "unknown"), otherwise returns the value itself
convertUnkToNothing "unknown" = Nothing
convertUnkToNothing s = Just s


-- |Gets the ID from any Star Wars API Url
--
--  It takes a single argument 
--
extractID :: Maybe [Char] -- ^ Takes a URL of type Maybe [Char]
          -> Maybe [Char] -- ^ returns the ID value as type [Char] or Nothing (if Nothing is passed)
extractID url = do
    case url of Nothing -> Nothing
                Just url -> Just [x | x <- url, isDigit x]

-- |Gets the head of a list
--
--  It returns Nothing if the list is empty
--
head' :: [a] -- ^ Takes a list of type a
      -> Maybe a -- ^ Returns the head of the list, or Nothing
head' []     = Nothing
head' (x:xs) = Just x


-- PLANETS
    

-- | Transforms Planet values to SQL
--
--  It takes a single argument Record of type Planet and Returns an Array of SQL Values
--

planetToSqlValues :: Planet -- ^ Takes a Record of type Planet
                  -> [SqlValue] -- ^ returns an array of SQL Values
planetToSqlValues planet = [
        toSql $ ParsePlanets.name planet,
        toSql $ convertUnkToNothing $ climate planet,
        toSql $ convertUnkToNothing $ diameter planet,
        toSql $ convertUnkToNothing $ population planet,
        toSql $ convertUnkToNothing $ terrain planet
    ]

-- | Prepares a Posgres statement that inserts values into the Planets table
--
--   It takes one argument and returns an IO  with a prepared HDBC Statement
--
prepareInsertPlanetSmt :: Connection -- ^ Takes a HDBC Connection
                       -> IO Statement -- ^ returns an IO with the prepared HDBC Statement 
prepareInsertPlanetSmt conn = prepare conn "INSERT INTO planets VALUES (?,?,?,?,?)"

-- | Saves values to the Planets table
--
--   It takes two arguments and returns an empty IO
--
savePlanets :: [Planet] -- ^ takes an array containing type 'Planet'
            -> Connection -- ^ takes an HDBC Connection
            -> IO () -- ^ Returns an empty IO
savePlanets planets conn = do
    stmt <- prepareInsertPlanetSmt conn
    executeMany stmt (map planetToSqlValues planets)
    commit conn


--  PEOPLE


-- | Transforms Person values to SQL
--
--  It takes a single argument Record of type Person and Returns an Array of SQL Values
--

personToSqlValues :: Person  -- ^ Takes a Record of type Person
                  -> [SqlValue] -- ^ returns an array of SQL Values
personToSqlValues person = [
        toSql $ ParsePeople.name person,
        toSql $ convertUnkToNothing $ gender person,
        toSql $ extractID $ Just (ParsePeople.homeworld person),
        toSql $ convertUnkToNothing $ height person,
        toSql $ convertUnkToNothing $ mass person,
        toSql $ extractID $ head' (ParsePeople.species person)
    ]

-- | Prepares a Posgres statement that inserts values into the People table
--
--   It takes one argument and returns an IO  with a prepared HDBC Statement
--
prepareInsertPeopleSmt :: Connection -- ^ Takes a HDBC Connection
                       -> IO Statement -- ^ returns an IO with the prepared HDBC Statement 
prepareInsertPeopleSmt conn = prepare conn "INSERT INTO people VALUES (?,?,?,?,?,?)"

-- | Saves values to the Person table
--
--   It takes two arguments and returns an empty IO
--
savePeople :: [Person] -- ^ takes an array containing type 'Person'
            -> Connection -- ^ takes an HDBC Connection
            -> IO () -- ^ Returns an empty IO
savePeople people conn = do
    stmt <- prepareInsertPeopleSmt conn
    executeMany stmt (map personToSqlValues people)
    commit conn


-- SPECIES


-- | Transforms Species values to SQL
--
--  It takes a single argument Record of type Species and Returns an Array of SQL Values
--
speciesToSqlValues :: Species  -- ^ Takes a Record of type Species
                   -> [SqlValue] -- ^ returns an array of SQL Values
speciesToSqlValues species = [
        toSql $ ParseSpecies.name species,
        toSql $ convertUnkToNothing $ classification species,
        toSql $ convertUnkToNothing $ language species,
        toSql $ extractID $ ParseSpecies.homeworld species
    ]


-- | Prepares a Posgres statement that inserts values into the Species table
--
--   It takes one argument and returns an IO  with a prepared HDBC Statement
--
prepareInsertSpeciesSmt :: Connection -- ^ Takes a HDBC Connection
                       -> IO Statement -- ^ returns an IO with the prepared HDBC Statement 
prepareInsertSpeciesSmt conn = prepare conn "INSERT INTO species VALUES (?,?,?,?)"

-- | Saves values to the Species table
--
--   It takes two arguments and returns an empty IO
--
saveSpecies :: [Species] -- ^ takes an array containing type 'Species'
            -> Connection -- ^ takes an HDBC Connection
            -> IO () -- ^ Returns an empty IO
saveSpecies species conn = do
    stmt <- prepareInsertSpeciesSmt conn
    executeMany stmt (map speciesToSqlValues species)
    commit conn


-- FILMS


-- | Transforms Film values to SQL
--
--  It takes a single argument Record of type Film and Returns an Array of SQL Values
--

filmToSqlValues :: Film  -- ^ Takes a Record of type Film
                -> [SqlValue] -- ^ returns an array of SQL Values
filmToSqlValues film = [
        toSql $ title film,
        toSql $ episode_id film,
        toSql $ opening_crawl film,
        toSql $ director film,
        toSql $ producer film,
        toSql $ release_date film
    ]


-- | Prepares a Posgres statement that inserts values into the Film table
--
--   It takes one argument and returns an IO  with a prepared HDBC Statement
--
prepareInsertFilmSmt :: Connection -- ^ Takes a HDBC Connection
                       -> IO Statement -- ^ returns an IO with the prepared HDBC Statement 
prepareInsertFilmSmt conn = prepare conn "INSERT INTO films VALUES (?,?,?,?,?,?)"

-- | Saves values to the Film table
--
--   It takes two arguments and returns an empty IO
--
saveFilms :: [Film] -- ^ takes an array containing type 'Film'
            -> Connection -- ^ takes an HDBC Connection
            -> IO () -- ^ Returns an empty IO
saveFilms films conn = do
    stmt <- prepareInsertFilmSmt conn
    executeMany stmt (map filmToSqlValues films)
    commit conn