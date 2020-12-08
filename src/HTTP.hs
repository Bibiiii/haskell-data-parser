-- | Module to Manage HTTP Requests
module HTTP
    ( download
    ) where

import qualified Data.ByteString.Lazy.Char8 as L8
import Network.HTTP.Simple
    ( parseRequest, getResponseBody, httpLBS )

-- | The "URL" type is for URls
type URL = String

-- | Downloads the response from a given URL

download :: URL -- ^ Takes a URL of type URL
         -> IO L8.ByteString -- ^ Returns a ByteString of the response body.
download url = do
    request <- parseRequest url
    response <- httpLBS request
    return $ getResponseBody response
