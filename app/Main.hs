module Main where

<<<<<<< Updated upstream
import ProcessJson
import Queries
=======
import ProcessJson ( processJson )
import Queries ( queryJoin, queryDelete, dropAllTables) 
>>>>>>> Stashed changes

main :: IO ()
main = do

    print "Dropping all tables....."
    dropAllTables
    print "All tables dropped"
    print "------------------------"

    processJson "https://swapi.dev/api/" "root"
    print "Parsing..."
    print "Done!"

<<<<<<< Updated upstream
    query 5
=======
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
>>>>>>> Stashed changes
