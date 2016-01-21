module Model.LeagueSettings where

import ClassyPrelude.Yesod

teamsCountOptions :: [(Text, Int)]
teamsCountOptions = toOptions possibleTeamCounts

possibleTeamCounts :: [Int]
possibleTeamCounts = [4..10]

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

possibleNumbersOfStarters :: [Int]
possibleNumbersOfStarters = [1..6]

possibleRosterSizes :: [Int]
possibleRosterSizes = [2..12]

possibleRegularSeasonLengths :: [Int]
possibleRegularSeasonLengths = [7..10]

possiblePlayoffLengths :: [Int]
possiblePlayoffLengths = [0..3]

possibleNumbersOfTeamsInPlayoffs :: Int -> [Int]
possibleNumbersOfTeamsInPlayoffs teamsCount = [2..teamsCount]

possibleTradeDeadlineWeeks :: [Int]
possibleTradeDeadlineWeeks = [7..10]

possibleWaiverPeriodsInDays :: [Int]
possibleWaiverPeriodsInDays = [0..2]

-- Default Values --
---------------------

-- takes in number of teams
-- returns (max # of starters, total # on team)
defaultRosterSize :: Int -> (Int, Int)
defaultRosterSize 4 = (5, 9) -- 4 on bench, 36 total on teams
defaultRosterSize 5 = (4, 7) -- 3 on bench, 35 total on teams
defaultRosterSize 6 = (3, 6) -- 3 on bench, 36 total on teams
defaultRosterSize 7 = (3, 5) -- 2 on bench, 35 total on teams
defaultRosterSize 8 = (3, 5) -- 2 on bench, 40 total on teams
defaultRosterSize 9 = (2, 4) -- 2 on bench, 36 total on teams
defaultRosterSize 10 = (2, 4) -- 2 on bench, 40 total on teams
defaultRosterSize _ = error "This roster size is not allowed"

-- takes in number of teams
-- returns (regular season length, playoff length)
defaultSeasonLength :: Int -> (Int, Int)
defaultSeasonLength teamsCount
    | teamsCount < 6 = (9, 1)
    | otherwise      = (8, 2)

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


