module DownloadJson
    (
        downloadJson
    ) where

import HTTP(download)
import qualified Data.ByteString.Lazy.Char8 as L8
import ParsePlanets
import Database ( initialiseDB, savePlanets )

parseJson :: L8.ByteString -> IO ()
parseJson json = do
    case parsePlanets json of
            Left err -> do
                print err
            Right res -> do
                conn <- initialiseDB
                case next res of
                    Just a -> do
                        savePlanets (results res) conn
                        downloadJson a
                    Nothing -> savePlanets (results res) conn
                
downloadJson :: String -> IO ()
downloadJson url = do
    print $ "Downloading and parsing url: " ++ url
    json <- download url
    parseJson json