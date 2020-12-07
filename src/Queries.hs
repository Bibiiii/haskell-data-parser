module Queries
    (
        query,
    ) where

import Database.HDBC
    ( toSql,
      fromSql,
      SqlValue,
      quickQuery',
      disconnect
    )

import Database.HDBC.PostgreSQL ( Connection, connectPostgreSQL )

query :: Int -> IO ()
query maxId = 
    do -- Connect to the database
       conn <- connectPostgreSQL "host=localhost dbname=postgres user=postgres password=admin"

       -- Run the query and store the results in r
       r <- quickQuery' conn
            -- "SELECT *, FROM people, WHERE person_id = maxId"
            "SELECT person_id from people where person_id <= ? ORDER BY person_id"
            [toSql maxId]

       -- Convert each row into a String
       let stringRows = map convRow r
                        
       -- Print the rows out
       mapM_ putStrLn stringRows

       -- And disconnect from the database
       disconnect conn

    where convRow :: [SqlValue] -> String
          convRow [sqlId, sqlDesc] = 
              show intid ++ ": " ++ desc
              where intid = fromSql sqlId::Integer
                    desc = case fromSql sqlDesc of
                             Just x -> x
                             Nothing -> "NULL"
          convRow x = fail $ "Unexpected result: " ++ show x


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