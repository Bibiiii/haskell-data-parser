module Main where

import ProcessJson
import Queries
import ProcessJson ( processJson )
import Queries ( dropAllTables, queryHomeworlds, queryDelete, queryPlanetWithHighestGround) 

main :: IO ()
main = do

    print "Dropping all tables....."
    dropAllTables
    print "All tables dropped"
    print "---------------------------------------------"

    processJson "https://swapi.dev/api/" "root"
    print "Parsing..."
    print "Done!"

    print "Performing homeworlds query - First 10 people and their home planets....."
    queryHomeworlds
    print "Done with homeworlds query!"
    print "---------------------------------------------"

    print "Performing delete query - Delete all people from Tatooine....."
    queryDelete
    print "Done with delete query!"
    print "---------------------------------------------"

    print "Testing delete query by performing homeworlds query again....."
    queryHomeworlds
    print "Done testing delete query by repeating homeworlds query!"
    print "---------------------------------------------"

    print "Planet with the highest ground...."
    queryPlanetWithHighestGround
    print "Found planet with highest ground"
    print "---------------------------------------------"
