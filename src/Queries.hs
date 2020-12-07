module Queries
    (
        query,
    ) where

import Database.HDBC
    ( toSql,
      fromSql,
      SqlValue,
      quickQuery'
    )

import Database.HDBC.PostgreSQL ( connectPostgreSQL )

getVal :: SqlValue -> String
getVal bytestrobj = fromSql bytestrobj :: String

query :: Int -> IO ()
query maxId = 
    do -- Connect to the database
        conn <- connectPostgreSQL "host=localhost dbname=postgres user=postgres password=admin"

        -- Run the query and store the results in r
        r <- quickQuery' conn
            -- "SELECT *, FROM people, WHERE person_id = maxId"
            "SELECT name FROM people WHERE person_id <= ?"
            [toSql maxId]

        let headResults = map head r
        let results = map getVal headResults
        
        mapM_ putStrLn results

-- queryOne :: IO Connection
-- queryOne = 
--     do
--         conn <- connectPostgreSQL "host=localhost dbname=postgres user=postgres password=admin"
--         r <- quickQuery' conn 
--             "SELECT * FROM people" [] 

--         -- Convert each row into a String
--        let stringRows = map convRow r

--        -- Print the rows out
--        mapM_ putStrLn stringRows

--        -- And disconnect from the database
--        disconnect conn 