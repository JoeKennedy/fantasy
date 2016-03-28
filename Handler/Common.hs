-- | Common handler functions.
module Handler.Common where

import Import

import Data.FileEmbed (embedFile)

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

------------------
-- Generic Form --
------------------
embeddedForm :: Route App -> Enctype -> Widget -> Widget
embeddedForm action enctype widget = $(widgetFile "embedded_form")

--------------------
-- Entity Helpers --
--------------------
extractValue :: (Entity t) -> t
extractValue (Entity _ value) = value

extractValueMaybe :: Maybe (Entity t) -> Maybe t
extractValueMaybe (Just (Entity _ value)) = Just value
extractValueMaybe Nothing                 = Nothing

--------------
-- Grouping --
--------------
groupByFirst :: Eq a => [(a, b)] -> [(a, [b])]
groupByFirst = map pairsToTuple . groupBy ((==) `on` fst)

groupByFirstOfFour :: Eq a => [(a, b, c, d)] -> [(a, [(b, c, d)])]
groupByFirstOfFour = groupByFirst . map quadrupleToTuple

groupByThirdOfFive :: Eq c => [(a, b, c, d, e)] -> [(c, [(a, b, d, e)])]
groupByThirdOfFive = groupByFirst . map quintupleToTuple3

groupByFirstOfSix :: Eq a => [(a, b, c, d, e, f)] -> [(a, [(b, c, d, e, f)])]
groupByFirstOfSix = groupByFirst . map sextupleToTuple

----------------------
-- Grouping Helpers --
----------------------
pairsToTuple :: [(a, b)] -> (a, [b])
pairsToTuple pairs = (fst . unsafeHead $ pairs, map snd pairs)

quadrupleToTuple :: (a, b, c, d) -> (a, (b, c, d))
quadrupleToTuple quadruple = (firstOf4 quadruple, last3Of4 quadruple)

quintupleToTuple :: (a, b, c, d, e) -> (a, (b, c, d, e))
quintupleToTuple quintuple = (firstOf5 quintuple, last4Of5 quintuple)

quintupleToTuple3 :: (a, b, c, d, e) -> (c, (a, b, d, e))
quintupleToTuple3 quintuple = (thirdOf5 quintuple, allBut3Of5 quintuple)

sextupleToTuple :: (a, b, c, d, e, f) -> (a, (b, c, d, e, f))
sextupleToTuple sextuple = (firstOf6 sextuple, last5Of6 sextuple)

firstOf4 :: (a, b, c, d) -> a
firstOf4 (x, _, _, _) = x

last3Of4 :: (a, b, c, d) -> (b, c, d)
last3Of4 (_, x, y, z) = (x, y, z)

firstOf5 :: (a, b, c, d, e) -> a
firstOf5 (x, _, _, _, _) = x

thirdOf5 :: (a, b, c, d, e) -> c
thirdOf5 (_, _, x, _, _) = x

allBut3Of5 :: (a, b, c, d, e) -> (a, b, d, e)
allBut3Of5 (v, w, _, y, z) = (v, w, y, z)

last4Of5 :: (a, b, c, d, e) -> (b, c, d, e)
last4Of5 (_, w, x, y, z) = (w, x, y, z)

firstOf6 :: (a, b, c, d, e, f) -> a
firstOf6 (x, _, _, _, _, _) = x

last5Of6 :: (a, b, c, d, e, f) -> (b, c, d, e, f)
last5Of6 (_, v, w, x, y, z) = (v, w, x, y, z)

-----------------
-- Conversions --
-----------------
intToOrdinal :: Int -> String
intToOrdinal int
    | int `div` 10 == 1 = show int ++ "th"
    | otherwise = case int `mod` 10 of 1 -> show int ++ "st"
                                       2 -> show int ++ "nd"
                                       3 -> show int ++ "rd"
                                       _ -> show int ++ "th"

