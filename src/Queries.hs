-- | Module which contains the queries
module Queries
    (   dropAllTables,
        queryDeletePeople,
        queryInsertToFilms,
        queryFilms,
        queryHomeworlds,
        queryPlanetWithHighestGround
    ) where

import Database.HDBC
    ( fromSql,
      toSql,
      quickQuery',
      SqlValue,
      IConnection(disconnect, commit) )
import Database.HDBC.PostgreSQL

-- | Gets the output as a string  
getVal :: SqlValue -- ^ Takes one argument, an SqlValue returned from a query
          -> String -- ^ Returns a string
getVal stringOutput = fromSql stringOutput :: String

-- | Converts a list of SqlValues into a list of strings for printing
formatStrings :: [SqlValue] -- ^ Takes one argument, a list SqlValues returned from a query
                 -> [String] -- ^ Returns a list of strings
formatStrings = map getVal

-- | Combines two lists of strings into a single list of strings, separated by a ": "
combine :: [[Char]] -> [[Char]] -- ^ Takes two arguments, both lists of strings
           -> [[Char]] -- ^ Returns a single list of strings
combine [] _ = []
combine _ [] = []
combine (x:xs) (y:ys) = (x ++ ": " ++ y) : combine xs ys

-- | Drops all tables from the database, ensures tables are always clean when re-running code
dropAllTables :: IO() -- ^ Performs an IO action
dropAllTables = 
    do
        conn <- connectPostgreSQL "host=localhost dbname=postgres user=postgres password=admin"
        quickQuery' conn
             "DROP TABLE IF EXISTS planets CASCADE"
             []

        quickQuery' conn
             "DROP TABLE IF EXISTS people CASCADE"
             []

        quickQuery' conn
             "DROP TABLE IF EXISTS species CASCADE"
             []
        
        quickQuery' conn
             "DROP TABLE IF EXISTS films CASCADE"
             []

        commit conn

        -- And disconnect from the database
        disconnect conn

-- | Query to find people and their homeworlds
queryHomeworlds :: IO () -- ^ Performs an IO action
queryHomeworlds =
    do
        conn <- connectPostgreSQL "host=localhost dbname=postgres user=postgres password=admin"
        r <- quickQuery' conn
             "SELECT people.name, planets.name FROM people INNER JOIN planets ON people.homeworld=planets.planet_id WHERE people.person_id <= 10"
             []
        let b = mapM formatStrings r
        mapM_ putStrLn $ combine (head b) (last b)
        disconnect conn

-- | Query to delete all droids
queryDeletePeople :: IO () -- ^ Performs an IO action
queryDeletePeople = 
    do -- Connect to the database
        conn <- connectPostgreSQL "host=localhost dbname=postgres user=postgres password=admin"

        -- Run the query
        quickQuery' conn
            "DELETE FROM people WHERE species = 2"
            []

        commit conn

        -- And disconnect from the database
        disconnect conn

-- | Query to insert a new row into the films table
queryInsertToFilms :: String -> String -> String -> String -> String -> String -- ^ Takes 6 strings as inputs
                      -> IO () -- ^ Performs an IO action
queryInsertToFilms filmName epId crawl director producer release_date = 
    do -- Connect to the database
        conn <- connectPostgreSQL "host=localhost dbname=postgres user=postgres password=admin"

        -- Run the query
        quickQuery' conn
            "INSERT INTO films VALUES (?,?,?,?,?,?)"
            [toSql filmName, toSql epId, toSql crawl,toSql director,toSql producer,toSql release_date]

        -- Commit the changes
        commit conn

        -- And disconnect from the database
        disconnect conn

-- | Query to retrive title and producer from the films table
queryFilms :: IO () -- ^ Performs an IO action
queryFilms = 
    do -- Connect to the database
        conn <- connectPostgreSQL "host=localhost dbname=postgres user=postgres password=admin"

        -- Run the query and store the results in r
        r <- quickQuery' conn
            "SELECT title, producer FROM films"
            []

        let b = mapM formatStrings r
        mapM_ putStrLn $ combine (head b) (last b)

        -- And disconnect from the database
        disconnect conn

-- | Query to find and return the planet with the highest ground
queryPlanetWithHighestGround :: IO () -- ^ Performs an IO action
queryPlanetWithHighestGround = 
    do -- Connect to the database
        conn <- connectPostgreSQL "host=localhost dbname=postgres user=postgres password=admin"

        -- Run the query and store the results in r
        r <- quickQuery' conn
            "SELECT name, diameter FROM planets WHERE diameter = (SELECT MAX(diameter) FROM planets)"
            []

        let b = mapM formatStrings r
        mapM_ putStrLn $ combine (head b) (last b)

        -- And disconnect from the database
        disconnect conn