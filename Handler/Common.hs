-- | Common handler functions.
module Handler.Common where

import           Data.FileEmbed  (embedFile)
import qualified Data.Map as Map (toList, fromListWith)
import           Import

-- These handlers embed files in the executable at compile time to avoid a
-- runtime dependency, and for efficiency.

getFaviconR :: Handler TypedContent
getFaviconR = return $ TypedContent "image/x-icon"
                     $ toContent $(embedFile "config/favicon.ico")

getRobotsR :: Handler TypedContent
getRobotsR = return $ TypedContent typePlain
                    $ toContent $(embedFile "config/robots.txt")

----------
-- USER --
----------
isAdmin :: Maybe (Entity User) -> Bool
isAdmin (Just (Entity _ user)) = userIsAdmin user
isAdmin Nothing                = False

-------------
-- Helpers --
-------------
groupByFirst :: Ord k => [(k, t)] -> Map k [t]
groupByFirst tuples = Map.fromListWith (++) [(x, [y]) | (x, y) <- tuples]

groupByThirdOfFive :: Ord c => [(a, b, c, d, e)] -> Map c [(a, b, d, e)]
groupByThirdOfFive quintuples =
    Map.fromListWith (++) $ reverse [(x, [(v, w, y, z)]) | (v, w, x, y, z) <- quintuples]

listByFirst :: Ord k => [(k, t)] -> [(k, [t])]
listByFirst tuples = Map.toList $ groupByFirst tuples

listByThirdOfFive :: Ord c => [(a, b, c, d, e)] -> [(c, [(a, b, d, e)])]
listByThirdOfFive quintuples = Map.toList $ groupByThirdOfFive quintuples

extractValue :: (Entity t) -> t
extractValue (Entity _ value) = value

extractValueMaybe :: Maybe (Entity t) -> Maybe t
extractValueMaybe (Just (Entity _ value)) = Just value
extractValueMaybe Nothing                 = Nothing

