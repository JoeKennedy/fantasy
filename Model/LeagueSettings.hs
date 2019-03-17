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

totalWeeksError :: a
totalWeeksError = error "Total weeks must be 6, 7, or 10"

----------------------
-- General Settings --
----------------------

-- Possible Values --
---------------------

possibleNumbersOfStarters :: Int -> [Int]
possibleNumbersOfStarters teamsCount = [1..fst $ maxRosterSize teamsCount]

possibleRosterSizes :: Int -> [Int]
possibleRosterSizes teamsCount = [2..snd $ maxRosterSize teamsCount]

possibleRegularSeasonLengths :: Int -> [Int]
possibleRegularSeasonLengths 10 = [7..10]
possibleRegularSeasonLengths  7 = [5..7]
possibleRegularSeasonLengths  6 = [4..6]
possibleRegularSeasonLengths  _ = totalWeeksError

possiblePlayoffLengths :: Int -> [Int]
possiblePlayoffLengths 10 = [0..3]
possiblePlayoffLengths  7 = [0..2]
possiblePlayoffLengths  6 = [0..2]
possiblePlayoffLengths  _ = totalWeeksError

possibleNumbersOfTeamsInPlayoffs :: Int -> [Int]
possibleNumbersOfTeamsInPlayoffs teamsCount = [2..teamsCount]

possibleTradeDeadlineWeeks :: Int -> [Int]
possibleTradeDeadlineWeeks 10 = [7..10]
possibleTradeDeadlineWeeks  7 = [5..7]
possibleTradeDeadlineWeeks  6 = [4..6]
possibleTradeDeadlineWeeks  _ = totalWeeksError

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

-- takes in total number of weeks in season and number of teams
-- returns (regular season length, playoff length)
defaultSeasonLength :: Int -> Int -> (Int, Int)
defaultSeasonLength 10 teamsCount
    | teamsCount < 6  = (9, 1)
    | teamsCount < 11 = (8, 2)
    | otherwise       = (7, 3)
defaultSeasonLength 7 _ = (7, 0)
defaultSeasonLength 6 _ = (6, 0)
defaultSeasonLength _ _ = totalWeeksError

-- takes in total number of weeks in season and number of teams
-- returns number of teams that make the playoffs
defaultNumberOfTeamsInPlayoffs :: Int -> Int -> Int
defaultNumberOfTeamsInPlayoffs 10 teamsCount = teamsCount `quot` 2
defaultNumberOfTeamsInPlayoffs 7 _ = 0
defaultNumberOfTeamsInPlayoffs 6 _ = 0
defaultNumberOfTeamsInPlayoffs _ _ = totalWeeksError

-- takes in total number of weeks in season and number of teams
-- returns last week of regular season
defaultTradeDeadlineWeek :: Int -> Int -> Int
defaultTradeDeadlineWeek 10 teamsCount = fst $ defaultSeasonLength 10 teamsCount
defaultTradeDeadlineWeek 7 _ = 6
defaultTradeDeadlineWeek 6 _ = 5
defaultTradeDeadlineWeek _ _ = totalWeeksError

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


