module Database
    ( initialiseDBPlanets,
      initialiseDBPeople, 
      savePlanets,
      savePeople
    ) where

import Database.HDBC
    ( toSql,
      SqlValue,
      Statement(executeMany),
      IConnection(commit, run, prepare) )
import Database.HDBC.PostgreSQL ( Connection, connectPostgreSQL )
import ParsePlanets
import ParsePeople

-- Create table for planets
initialiseDBPlanets :: IO Connection
initialiseDBPlanets =
    do
        conn <- connectPostgreSQL "host=localhost dbname=postgres user=postgres password=admin"
        run conn "CREATE TABLE IF NOT EXISTS planets (\
            \name VARCHAR(40) NOT NULL, \
            \rotation_period INT, \
            \orbital_period INT, \
            \diameter INT, \
            \climate VARCHAR(40), \
            \gravity VARCHAR(40), \
            \terrain VARCHAR(40), \
            \surface_water FLOAT(2), \
            \population BIGINT \
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
            \name VARCHAR(40) NOT NULL, \
            \gender VARCHAR(40), \
            \hair_color VARCHAR(40) \
            \)\
            \" []
        commit conn
        return conn

-- convert any 'unknown' values to Nothing
convertUnkToNothing :: String -> Maybe String
convertUnkToNothing "unknown" = Nothing
convertUnkToNothing s = Just s

-- transform planet values to SQL
planetToSqlValues :: Planet -> [SqlValue]
planetToSqlValues planet = [
        toSql $ ParsePlanets.name planet,
        toSql $ convertUnkToNothing $ rotation_period planet,
        toSql $ convertUnkToNothing $ orbital_period planet,
        toSql $ convertUnkToNothing $ diameter planet,
        toSql $ convertUnkToNothing $ climate planet,
        toSql $ convertUnkToNothing $ gravity planet,
        toSql $ convertUnkToNothing $ terrain planet,
        toSql $ convertUnkToNothing $ surface_water planet,
        toSql $ convertUnkToNothing $ population planet
    ]

prepareInsertPlanetSmt :: Connection -> IO Statement
prepareInsertPlanetSmt conn = prepare conn "INSERT INTO planets VALUES (?,?,?,?,?,?,?,?,?)"

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
        toSql $ convertUnkToNothing $ hair_color person
    ]

prepareInsertPeopleSmt :: Connection -> IO Statement
prepareInsertPeopleSmt conn = prepare conn "INSERT INTO people VALUES (?,?,?)"

savePeople :: [Person] -> Connection -> IO ()
savePeople people conn = do
    stmt <- prepareInsertPeopleSmt conn
    executeMany stmt (map personToSqlValues people)
    commit conn