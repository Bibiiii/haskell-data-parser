module Database
    ( initialiseDB,
      savePlanets
    ) where

import Database.HDBC
import Database.HDBC.PostgreSQL
import Parse

initialiseDB :: IO Connection
initialiseDB =
    do
        conn <- connectPostgreSQL "host=localhost dbname=postgres user=postgres password=admin"
        run conn "CREATE TABLE IF NOT EXISTS planets (\
            \name VARCHAR(40) NOT NULL, \
            \rotation_period VARCHAR(40) NOT NULL, \
            \orbital_period VARCHAR(40) NOT NULL, \
            \diameter VARCHAR(40) NOT NULL, \
            \climate VARCHAR(40) NOT NULL, \
            \gravity VARCHAR(40) NOT NULL, \
            \terrain VARCHAR(40) NOT NULL, \
            \surface_water VARCHAR(40) NOT NULL, \
            \population VARCHAR(40) NOT NULL \
            \)" []
        commit conn
        return conn

planetToSqlValues :: Planet -> [SqlValue]
planetToSqlValues planet = [
        toSql $ name planet,
        toSql $ rotation_period planet,
        toSql $ orbital_period planet,
        toSql $ diameter planet,
        toSql $ climate planet,
        toSql $ gravity planet,
        toSql $ terrain planet,
        toSql $ surface_water planet,
        toSql $ population planet
    ]

prepareInsertPlanetSmt :: Connection -> IO Statement
prepareInsertPlanetSmt conn = prepare conn "INSERT INTO planets VALUES (?,?,?,?,?,?,?,?,?)"

savePlanets :: [Planet] -> Connection -> IO ()
savePlanets planets conn = do
    stmt <- prepareInsertPlanetSmt conn
    executeMany stmt (map planetToSqlValues planets)
    commit conn