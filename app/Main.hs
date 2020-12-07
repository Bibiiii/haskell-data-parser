module Main where

import ProcessJson
import Queries

main :: IO ()
main = do
    -- processJson "https://swapi.dev/api/" "root"
    print "Parsing..."
    print "Done!"
    print "Querying DB..."
    query 5