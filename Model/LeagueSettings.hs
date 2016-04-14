module Model.LeagueSettings where

import ClassyPrelude.Yesod

teamsCountOptions :: Maybe Int -> [(Text, Int)]
teamsCountOptions (Just teamsCount) = toOptions [teamsCount]
teamsCountOptions Nothing = toOptions possibleTeamCounts

teamsCountOption :: Int -> [(Text, Int)]
teamsCountOption teamsCount = toOptions [teamsCount]

possibleTeamCounts :: [Int]
possibleTeamCounts = [4..12]

---------------------
-- Generic Helpers --
---------------------

toOptions :: (Show a) => [a] -> [(Text, a)]
toOptions valuesList = map (\value -> (pack $ show value, value)) valuesList

----------------------
-- General Settings --
----------------------

-- Possible Values --
---------------------

possibleNumbersOfStarters :: Int -> [Int]
possibleNumbersOfStarters teamsCount = [1..fst $ maxRosterSize teamsCount]

possibleRosterSizes :: Int -> [Int]
possibleRosterSizes teamsCount = [2..snd $ maxRosterSize teamsCount]

possibleRegularSeasonLengths :: [Int]
possibleRegularSeasonLengths = [7..10]

possiblePlayoffLengths :: [Int]
possiblePlayoffLengths = [0..3]

possibleNumbersOfTeamsInPlayoffs :: Int -> [Int]
possibleNumbersOfTeamsInPlayoffs teamsCount = [2..teamsCount]

possibleTradeDeadlineWeeks :: [Int]
possibleTradeDeadlineWeeks = [7..10]

possibleWaiverPeriodsInDays :: [Int]
possibleWaiverPeriodsInDays = [0..3]

-- Default Values --
---------------------

-- takes in number of teams
-- returns (max # of starters, total # on team)
defaultRosterSize :: Int -> (Int, Int)
defaultRosterSize  4 = (5, 9) -- 4 on bench, 36 total on teams
defaultRosterSize  5 = (4, 7) -- 3 on bench, 35 total on teams
defaultRosterSize  6 = (3, 6) -- 3 on bench, 36 total on teams
defaultRosterSize  7 = (3, 5) -- 2 on bench, 35 total on teams
defaultRosterSize  8 = (3, 5) -- 2 on bench, 40 total on teams
defaultRosterSize  9 = (2, 4) -- 2 on bench, 36 total on teams
defaultRosterSize 10 = (2, 4) -- 2 on bench, 40 total on teams
defaultRosterSize 11 = (2, 3) -- 1 on bench, 33 total on teams
defaultRosterSize 12 = (2, 3) -- 1 on bench, 36 total on teams
defaultRosterSize _  = error "This roster size is not allowed"

-- takes in number of teams
-- returns (regular season length, playoff length)
defaultSeasonLength :: Int -> (Int, Int)
defaultSeasonLength teamsCount
    | teamsCount < 6  = (9, 1)
    | teamsCount < 11 = (8, 2)
    | otherwise       = (7, 3)

-- takes in number of teams
-- returns number of teams that make the playoffs
defaultNumberOfTeamsInPlayoffs :: Int -> Int
defaultNumberOfTeamsInPlayoffs teamsCount = teamsCount `quot` 2

-- takes in number of teams
-- returns last week of regular season
defaultTradeDeadlineWeek :: Int -> Int
defaultTradeDeadlineWeek teamsCount = fst $ defaultSeasonLength teamsCount

defaultWaiverPeriodInDays :: Int
defaultWaiverPeriodInDays = 1

-- Helpers --
-------------

-- takes in number of teams
-- returns (max # of starters, max # on team)
maxRosterSize :: Int -> (Int, Int)
maxRosterSize  4 = (10, 16) -- 64 total players on rosters
maxRosterSize  5 = ( 8, 13) -- 65 total players on rosters
maxRosterSize  6 = ( 7, 11) -- 66 total players on rosters
maxRosterSize  7 = ( 6,  9) -- 63 total players on rosters
maxRosterSize  8 = ( 5,  8) -- 64 total players on rosters
maxRosterSize  9 = ( 4,  7) -- 63 total players on rosters
maxRosterSize 10 = ( 4,  6) -- 60 total players on rosters
maxRosterSize 11 = ( 4,  6) -- 66 total players on rosters
maxRosterSize 12 = ( 3,  5) -- 60 total players on rosters
maxRosterSize _  = error "This roster size is not allowed"


--------------------
-- Draft Settings --
--------------------

-- Possible Values --
---------------------

possibleSecondsPerPick :: [Int]
possibleSecondsPerPick = [30, 60, 90, 120, 150]

-- Default values --
--------------------

defaultSecondsPerPick :: Int
defaultSecondsPerPick = 90


