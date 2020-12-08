module Main where

import ProcessJson
import Queries
import ProcessJson ( processJson )
import Queries ( queryJoin, queryDelete, dropAllTables) 

main :: IO ()
main = do

    print "Dropping all tables....."
    dropAllTables
    print "All tables dropped"
    print "------------------------"

    processJson "https://swapi.dev/api/" "root"
    print "Parsing..."
    print "Done!"

    print "Performing first query - First 10 people and their home planets....."
    queryJoin
    print "Done with first query!"
    print "------------------------"

    print "Performing delete query....."
    queryDelete
    print "Done with delete query!"
    print "------------------------"

    print "Testing delete query....."
    queryJoin

