module Database
    ( initialiseDB,
      savePlanets
    ) where

import Database.HDBC
import Database.HDBC.PostgreSQL
import ParsePlanets

initialiseDB :: IO Connection
initialiseDB =
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
            \)" []
        commit conn
        return conn


convertUnkToNothing :: String -> Maybe String
convertUnkToNothing "unknown" = Nothing
convertUnkToNothing s = Just s
    
planetToSqlValues :: Planet -> [SqlValue]
planetToSqlValues planet = [
        toSql $ name planet,
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

savePlanets :: [Planet] -> Connection -> IO ()
savePlanets planets conn = do
    stmt <- prepareInsertPlanetSmt conn
    executeMany stmt (map planetToSqlValues planets)
    commit conn