
-- module Parse
--     ( parse,
--       Results ( results ),
--       Planet ( name, rotation_period, orbital_period, diameter, climate, gravity, terrain, surface_water, population )
--     ) where

-- import Data.Aeson ( eitherDecode, FromJSON, ToJSON )
-- import qualified Data.ByteString.Lazy.Char8 as L8
-- import GHC.Generics

-- data Planet = Planet {
--             name :: String,
--             rotation_period :: String,
--             orbital_period :: String,
--             diameter :: String,
--             climate :: String,
--             gravity :: String,
--             terrain :: String,
--             surface_water :: String,
--             population ::  String
--         } deriving (Show, Generic)

-- instance FromJSON Planet
-- instance ToJSON Planet

-- data SomethingElse = SomethingElse {
--                       somethingName :: String,
--                       somethingPop :: String
--                   } deriving (Show, Generic)

-- instance FromJSON SomethingElse
-- instance ToJSON SomethingElse

-- data Thing = CasePlanet Planet | CaseSomethingElse SomethingElse deriving Show

-- data Results = Results {
--             -- count :: Int,
--             next :: String,
--             -- previous :: Maybe String,
--             results :: [Planet]
--         } deriving (Show, Generic)

-- instance FromJSON Results
-- instance ToJSON Results

-- instance FromJSON Results where
--   FromJSON S = if isPlanet S then CasePlanet $ FromJSONPlanet S
--               else CaseSomethingElse $ FromJSONOther S
-- instance ToJSON Results
    
-- parse :: L8.ByteString -> Either String Results
-- parse json = eitherDecode json :: Either String Results