module Handler.League.Week where

import Import

--------------
-- Creators --
--------------
createWeekData :: Entity Episode -> LeagueId -> Handler ()
createWeekData (Entity episodeId episode) leagueId = do
    -- create week if week not already created for episode
    alreadyCreatedWeek <- runDB $ getBy $ UniqueWeekLeagueIdEpisodeId leagueId episodeId
    case alreadyCreatedWeek of
        Just _ -> return ()
        Nothing -> do
            weekId <- createWeek leagueId episodeId $ episodeNumber episode
            -- then, for the week:
            teamIds <- runDB $ selectKeysList [TeamLeagueId ==. leagueId] []
            -- create game for each team in league
            mapM_ (createGame leagueId weekId) teamIds
            -- create performance for each player in league
            players <- runDB $ selectList [PlayerLeagueId ==. leagueId] []
            mapM_ (createPerformance leagueId weekId) players

createWeek :: LeagueId -> EpisodeId -> Int -> Handler WeekId
createWeek leagueId episodeId episodeNo = do
    now <- liftIO getCurrentTime
    runDB $ insert $ Week { weekLeagueId  = leagueId
                          , weekEpisodeId = episodeId
                          , weekNumber    = episodeNo
                          , weekIsScored  = False
                          , weekCreatedAt = now
                          , weekUpdatedAt = now
                          }

createGame :: LeagueId -> WeekId -> TeamId -> Handler ()
createGame leagueId weekId teamId = do
    now <- liftIO getCurrentTime
    runDB $ insert_ $ Game { gameLeagueId  = leagueId
                           , gameWeekId    = weekId
                           , gameTeamId    = teamId
                           , gamePoints    = 0
                           , gameCreatedAt = now
                           , gameUpdatedAt = now
                           }

createPerformance :: LeagueId -> WeekId -> Entity Player -> Handler ()
createPerformance leagueId weekId (Entity playerId player) = do
    now <- liftIO getCurrentTime
    runDB $ insert_ $ Performance { performanceLeagueId  = leagueId
                                  , performanceWeekId    = weekId
                                  , performancePlayerId  = playerId
                                  , performanceTeamId    = playerTeamId player
                                  , performanceIsStarter = playerIsStarter player
                                  , performancePoints    = 0
                                  , performanceCreatedAt = now
                                  , performanceUpdatedAt = now
                                  }


