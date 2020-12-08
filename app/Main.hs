module Main where

import ProcessJson
import Queries

main :: IO ()
main = do
    print "Dropping all tables....."
    dropAllTables
    print "All tables dropped"
    print "------------------------"

    processJson "https://swapi.dev/api/" "root"
    print "Parsing..."
    print "Done!"

    print "Performing first query - SELECT first 10 people and their home planets....."
    queryJoin
    print "Done with first query!"
    print "------------------------"

    print "Performing delete query - DELETE Luke Skywalker ....."
    queryDelete
    print "Done with delete query!"
    print "------------------------"

    print "Testing delete query....."
    queryJoin

    print "Adding new film"
    queryInsert "Haskell Strikes Back" "10" "On Earth, 4 team members try to write a Star Wars parser in Haskell" "Paulo Oliva & Edmund Robinson" "Armaan, Cat, Luca and Luca" "2020-12-10"
    queryFilms
