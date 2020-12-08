module Main where

import ProcessJson
import Queries

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

    print "Adding new film..."
    queryInsert "Haskell Strikes Back" "10" filmTagline "Paulo Oliva & Edmund Robinson" "Armaan, Cat, Luca and Luca" "2020-12-10"
    print "Returning all films..."
    queryFilms
    print "---------------------------------------------"

    print "Planet with the highest ground...."
    queryPlanetWithHighestGround
    print "Found planet with highest ground"
    print "---------------------------------------------"

    print "All queries complete, have a nice day :)"

filmTagline :: [Char]
filmTagline = "Beset on all sides by QMUL deadlines,\n\
            \the student forces stationed on Earth are crumbling under the relentless assault.\n \
            \Unwilling to let such an important system fall into QMUL's hands,\n \
            \the student council dispatches four of their most powerful members;\n \
            \Jedi Masters Armaan, Catherine, Luca & Luca,\n \
            \to reinforce the student base on Earth, as they attempt to build a parser in Haskell.\n \
            \But with only 10 rotations until the deadline fleet arrives, time is running out....."