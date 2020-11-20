module Main where

import HTTP
import Parse
import Database

main :: IO ()
main = do
    let url = "https://swapi.dev/api/planets/"
    json <- download url
    print "Parsing..."
    case (parse json) of
        Left err -> print err
        Right res -> do
            print "Saving on DB..."
            conn <- initialiseDB
            savePlanets (results res) conn
    print "Done!"