module HTTP
    ( download
    ) where

import qualified Data.ByteString.Lazy.Char8 as L8
import Network.HTTP.Simple
    ( parseRequest, getResponseBody, httpLBS )

-- | The "URL" type is for URls
type URL = String

{- | "download" gets a response from a given URL
    It takes one argument:
    - a URL of type URL
    It returns a ByteString of the response body
-}
download :: URL -> IO L8.ByteString
download url = do
    request <- parseRequest url
    response <- httpLBS request
    return $ getResponseBody response
