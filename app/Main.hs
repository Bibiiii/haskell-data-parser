module Main where

import DownloadJson

main :: IO ()
main = do
    downloadJson "https://swapi.dev/api/planets/"
    print "Parsing..."
    print "Done!"