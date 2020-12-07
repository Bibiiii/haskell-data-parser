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
            "SELECT name, height, homeworld FROM people WHERE person_id <= ?"
            [toSql maxId]

        print r

        print "-----------------"

        let headResults = map (!!0) r
        let h = map (!!1) r
        let tailResults = map last r

        -- print headResults
        -- print tailResults
        let nameResults = map getVal headResults
        let heightResults = map getVal h
        let homeResults = map getVal tailResults
        
        mapM_ putStrLn nameResults
        mapM_ putStrLn heightResults
        mapM_ putStrLn homeResults

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