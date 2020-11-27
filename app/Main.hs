module Main where

import ProcessJson

main :: IO ()
main = do
    processJson "https://swapi.dev/api/" "root"
    print "Parsing..."
    print "Done!"