module Queries
    (   dropAllTables,
        queryDelete,
        queryInsert,
        queryFilms,
        queryHomeworlds,
        queryPlanetWithHighestGround
    ) where

import Database.HDBC
import Database.HDBC.PostgreSQL

getVal :: SqlValue -> String
getVal stringOutput = fromSql stringOutput :: String

formatStrings :: [SqlValue] -> [String]
formatStrings = map getVal

combine :: [[Char]] -> [[Char]] -> [[Char]]
combine [] _ = []
combine _ [] = []
combine (x:xs) (y:ys) = (x ++ ": " ++ y) : combine xs ys

dropAllTables :: IO()
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

queryHomeworlds :: IO ()
queryHomeworlds =
    do
        conn <- connectPostgreSQL "host=localhost dbname=postgres user=postgres password=admin"
        r <- quickQuery' conn
             "SELECT people.name, planets.name FROM people INNER JOIN planets ON people.homeworld=planets.planet_id WHERE people.person_id <= 10"
             []
        let b = mapM formatStrings r
        mapM_ putStrLn $ combine (head b) (last b)
        disconnect conn

queryDelete :: IO ()
queryDelete = 
    do -- Connect to the database
        conn <- connectPostgreSQL "host=localhost dbname=postgres user=postgres password=admin"

        -- Run the query
        quickQuery' conn
            "DELETE FROM people WHERE homeworld = 1"
            []

        commit conn

        -- And disconnect from the database
        disconnect conn

queryInsert :: String -> String -> String -> String -> String -> String -> IO ()
queryInsert filmName epId crawl director producer release_date = 
    do -- Connect to the database
        conn <- connectPostgreSQL "host=localhost dbname=postgres user=postgres password=admin"

        -- Run the query
        quickQuery' conn
            "INSERT INTO films VALUES (?,?,?,?,?,?)"
            [toSql filmName, toSql epId, toSql crawl,toSql director,toSql producer,toSql release_date]

        commit conn

        -- And disconnect from the database
        disconnect conn

queryFilms :: IO ()
queryFilms = 
    do -- Connect to the database
        conn <- connectPostgreSQL "host=localhost dbname=postgres user=postgres password=admin"

        -- Run the query
        r <- quickQuery' conn
            "SELECT title, producer FROM films"
            []

        let b = mapM formatStrings r
        mapM_ putStrLn $ combine (head b) (last b)

        -- And disconnect from the database
        disconnect conn

queryPlanetWithHighestGround :: IO ()
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