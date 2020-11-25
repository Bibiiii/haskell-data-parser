module Main where

import ProcessJson

main :: IO ()
main = do
    processJson "https://swapi.dev/api/planets/"
    print "Parsing..."
    print "Done!"