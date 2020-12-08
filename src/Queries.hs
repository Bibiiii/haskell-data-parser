module Queries
    (   dropAllTables,
        queryJoin,
        queryDelete
    ) where

import Database.HDBC
    ( toSql,
      fromSql,
      SqlValue,
      quickQuery',
      disconnect
    )

import Database.HDBC.PostgreSQL ( Connection, connectPostgreSQL )

import Database.HDBC
    ( fromSql, quickQuery', SqlValue, IConnection(disconnect, commit) )

import Database.HDBC.PostgreSQL ( connectPostgreSQL )

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

queryJoin :: IO ()
queryJoin =
    do
        conn <- connectPostgreSQL "host=localhost dbname=postgres user=postgres password=admin"
        r <- quickQuery' conn
             "SELECT people.name, planets.name FROM people INNER JOIN planets ON people.homeworld=planets.planet_id WHERE people.person_id <=10"
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
            "DELETE FROM people WHERE person_id = 1"
            []

        commit conn

        -- And disconnect from the database
        disconnect conn
