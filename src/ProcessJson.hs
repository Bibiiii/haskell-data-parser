module ProcessJson
    (
        processJson
    ) where

import HTTP(download)
import qualified Data.ByteString.Lazy.Char8 as L8
import ParsePlanets
import Database ( initialiseDB, savePlanets )

saveJson :: PlanetResults -> IO ()
saveJson res = do
    conn <- initialiseDB
    case next res of
        Just a -> do
            savePlanets (results res) conn
            processJson a
        Nothing -> savePlanets (results res) conn

parseJson :: L8.ByteString -> IO ()
parseJson json = do
    case parsePlanets json of
            Left err -> do
                print err
            Right res -> do
                saveJson res

processJson :: String -> IO ()
processJson url = do
    print $ "Downloading and parsing url: " ++ url
    json <- download url
    parseJson json