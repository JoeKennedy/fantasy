module Handler.Score where

import Import

import qualified Database.Persist.Sql as S (fromSqlKey)
import qualified Database.Esqueleto   as E
import           Database.Esqueleto   ((^.))

-------------
-- Queries --
-------------
getGamesForTeam :: SeasonId -> TeamId -> Handler [(Entity Game, Entity Week)]
getGamesForTeam seasonId teamId = runDB
    $ E.select
    $ E.from $ \(game `E.InnerJoin` week) -> do
        E.on $ game ^. GameWeekId E.==. week ^. WeekId
        E.where_ $ game ^. GameTeamId E.==. E.val teamId
            E.&&. week ^. WeekSeasonId E.==. E.val seasonId
        E.orderBy [E.asc (week ^. WeekNumber)]
        return (game, week)

getPerformancesForPlayer :: SeasonId -> PlayerId -> Handler [(Entity Performance, Entity Week)]
getPerformancesForPlayer seasonId playerId = runDB
    $ E.select
    $ E.from $ \(performance `E.InnerJoin` week) -> do
        E.on $ performance ^. PerformanceWeekId E.==. week ^. WeekId
        E.where_ $ performance ^. PerformancePlayerId E.==. E.val playerId
            E.&&. week ^. WeekSeasonId E.==. E.val seasonId
        E.orderBy [E.asc (week ^. WeekNumber)]
        return (performance, week)

getPreviousWeeks :: Week -> Episode -> Handler [Entity Week]
getPreviousWeeks week episode = runDB $ do
    series <- get404 $ episodeSeriesId episode
    let overallNumber = episodeOverallNumber episode
    E.select $ E.from $ \(week' `E.InnerJoin` episode') -> do
        E.on $ week' ^. WeekEpisodeId E.==. episode' ^. EpisodeId
        E.where_ $ week' ^. WeekLeagueId E.==. E.val (weekLeagueId week)
            E.&&. episode' ^. EpisodeOverallNumber E.<. E.val overallNumber
        E.orderBy [E.desc (episode' ^. EpisodeOverallNumber)]
        -- TODO - maybe this number should be configurable by league?
        -- But right now it's the total number of episodes in the season
        E.limit $ fromIntegral $ seriesTotalEpisodes series
        return week'

--------------------
-- Finalize Weeks --
--------------------
finalizeWeek :: EpisodeId -> UserId -> LeagueId -> Handler ()
finalizeWeek episodeId userId leagueId = do
    Entity weekId week <- runDB $ getBy404 $ UniqueWeekLeagueIdEpisodeId leagueId episodeId
    calculatePointsThisSeason week userId
    calculateNextWeekCumulativePoints week
    determineWaiverOrder week userId
    moveSeasonToPostSeason week userId
    markWeekAsScored $ Entity weekId week
    -- TODO - Implement the below function and un-comment the below line
    -- emailTeamOwners episode -- Email only if league draft is complete

-- TODO - delete this function
unfinalizeWeek :: EpisodeId -> LeagueId -> Handler ()
unfinalizeWeek episodeId leagueId = do
    weekEntity <- runDB $ getBy404 $ UniqueWeekLeagueIdEpisodeId leagueId episodeId
    calculateWeekCumulativePoints weekEntity

markWeekAsScored :: Entity Week -> Handler ()
markWeekAsScored (Entity weekId week) = if weekIsScored week then return () else do
    now <- liftIO getCurrentTime
    runDB $ update weekId [WeekIsScored =. True, WeekUpdatedAt =. now]

determineWaiverOrder :: Week -> UserId -> Handler ()
determineWaiverOrder week userId = runDB $ do
    -- order teams by points (in regular or postseason) and make that the waiver order
    let conditions = if weekIsPostSeason week
                         then [ Asc  TeamSeasonPostSeasonStatus
                              , Asc  TeamSeasonPostSeasonPoints
                              , Asc  TeamSeasonRegularSeasonPoints]
                         else [ Asc  TeamSeasonRegularSeasonPoints
                              , Desc TeamSeasonDraftOrder]
    teamSeasonIds <- selectKeysList [TeamSeasonSeasonId ==. weekSeasonId week] conditions
    mapM_ (updateWaiverOrder userId) $ rank teamSeasonIds

updateWaiverOrder :: UserId -> (Int, TeamSeasonId) -> ReaderT SqlBackend Handler ()
updateWaiverOrder userId (waiverOrder, teamSeasonId) = do
    now <- liftIO getCurrentTime
    update teamSeasonId [ TeamSeasonWaiverOrder =. waiverOrder
                        , TeamSeasonUpdatedBy =. userId
                        , TeamSeasonUpdatedAt =. now ]

moveSeasonToPostSeason :: Week -> UserId -> Handler ()
moveSeasonToPostSeason week userId = do
    -- move league to postseason if this week is the last week of regular season
    lastRegularSeasonWeek <- isLastRegularSeasonWeek week
    if not lastRegularSeasonWeek then return () else runDB $ do
        let seasonId = weekSeasonId week
        -- first, split teams into playoffs and consolation
        teamSeasonIds <- selectKeysList [TeamSeasonSeasonId ==. seasonId]
                                        [Desc TeamSeasonRegularSeasonPoints]
        Entity _ generalSettings <- getBy404 $ UniqueGeneralSettingsSeasonId seasonId 
        let teamsInPlayoffs = generalSettingsNumberOfTeamsInPlayoffs generalSettings
            (playoffTSIds, consolationTSIds) = splitAt teamsInPlayoffs teamSeasonIds
        mapM_ (moveTeamSeasonToPostSeason Playoff userId) playoffTSIds
        mapM_ (moveTeamSeasonToPostSeason Consolation userId) consolationTSIds

        -- then, set league to being in postseason
        now <- liftIO getCurrentTime
        update seasonId [ SeasonIsInPostSeason =. True
                        , SeasonUpdatedBy =. userId
                        , SeasonUpdatedAt =. now ]

moveTeamSeasonToPostSeason :: PostSeasonStatus -> UserId -> TeamSeasonId ->
                              ReaderT SqlBackend Handler ()
moveTeamSeasonToPostSeason postSeasonStatus userId teamSeasonId = do
    now <- liftIO getCurrentTime
    update teamSeasonId [ TeamSeasonPostSeasonStatus =. postSeasonStatus
                        , TeamSeasonUpdatedBy =. userId
                        , TeamSeasonUpdatedAt =. now ]


-----------
-- Plays --
-----------
updatePlayNotes :: EventId -> Event -> Handler ()
updatePlayNotes eventId event = runDB $ do
    now <- liftIO getCurrentTime
    updateWhere [PlayEventId ==. eventId]
                [PlayNote =. eventNote event, PlayUpdatedAt =. now]

upsertPlays :: Episode -> Entity Event -> Handler ()
upsertPlays episode eventEntity = do
    seasons <- runDB $ selectList [ SeasonIsActive ==. True
                                  , SeasonSeriesId ==. episodeSeriesId episode
                                  ] [Asc SeasonId]
    mapM_ (upsertPlay eventEntity) $ map (seasonLeagueId . entityVal) seasons

upsertPlay :: Entity Event -> LeagueId -> Handler ()
upsertPlay (Entity eventId event) leagueId = do
    let episodeId = eventEpisodeId event
    Entity weekId week <- runDB $ getBy404 $ UniqueWeekLeagueIdEpisodeId leagueId episodeId
    maybePlayEntity <- runDB $ getBy $ UniquePlayWeekIdEventId weekId eventId
    case maybePlayEntity of
        Nothing -> createPlay leagueId (Entity weekId week) $ Entity eventId event
        Just playEntity -> updatePlay week event playEntity

createPlay :: LeagueId -> Entity Week -> Entity Event -> Handler ()
createPlay leagueId (Entity weekId week) (Entity eventId event) = do
    (aPoints, wPoints, aRecPnts, wRecPnts, playerId, mRecPlayerId) <- calculatePointsAndPlayers leagueId (Entity weekId week) event
    now <- liftIO getCurrentTime

    let play = Play { playLeagueId     = leagueId
                    , playWeekId       = weekId
                    , playEventId      = eventId
                    , playPlayerId     = playerId
                    , playPoints       = aPoints + wPoints
                    , playActionPoints = aPoints
                    , playWeightPoints = wPoints
                    , playAction       = eventAction event
                    , playReceivingPlayerId     = mRecPlayerId
                    , playReceivingPoints       = aRecPnts + wRecPnts
                    , playReceivingActionPoints = aRecPnts
                    , playReceivingWeightPoints = wRecPnts
                    , playNote = eventNote event
                    , playCreatedAt = now
                    , playUpdatedAt = now
                    }
    runDB $ insert_ play
    updatePointsFromPlay play False

updatePlay :: Week -> Event -> Entity Play -> Handler ()
updatePlay week event (Entity playId play) = do
    let (leagueId, weekId) = (playLeagueId play, playWeekId play)
    (aPoints, wPoints, aRecPnts, wRecPnts, playerId, mRecPlayerId) <- calculatePointsAndPlayers leagueId (Entity weekId week) event
    now <- liftIO getCurrentTime
    updatePointsFromPlay play True

    play' <- runDB $ updateGet playId [ PlayPlayerId     =. playerId
                                      , PlayPoints       =. aPoints + wPoints
                                      , PlayActionPoints =. aPoints
                                      , PlayWeightPoints =. wPoints
                                      , PlayAction       =. eventAction event
                                      , PlayReceivingPlayerId     =. mRecPlayerId
                                      , PlayReceivingPoints       =. aRecPnts + wRecPnts
                                      , PlayReceivingActionPoints =. aRecPnts
                                      , PlayReceivingWeightPoints =. wRecPnts
                                      , PlayNote      =. eventNote event
                                      , PlayUpdatedAt =. now
                                      ]
    updatePointsFromPlay play' False

deleteEvent :: EventId -> Handler ()
deleteEvent eventId = do
    plays <- runDB $ selectList [PlayEventId ==. eventId] []
    mapM_ deletePlay plays
    runDB $ delete eventId

deletePlay :: Entity Play -> Handler ()
deletePlay (Entity playId play) = do
    updatePointsFromPlay play True
    runDB $ delete playId


----------------------
-- Calculate Points --
----------------------
calculatePointsAndPlayers :: LeagueId -> Entity Week -> Event ->
                             Handler (Rational, Rational, Rational, Rational, PlayerId, Maybe PlayerId)
calculatePointsAndPlayers leagueId (Entity weekId week) event = runDB $ do
    let (charId, recCharId) = (eventCharacterId event, eventReceivingCharacterId event)
    Entity playerId _ <- getBy404 $ UniquePlayerLeagueIdCharacterId leagueId charId
    mRecPlayer <- mapM (getBy404 . UniquePlayerLeagueIdCharacterId leagueId) recCharId

    let recPlayerId = map entityKey mRecPlayer
    Entity _ performance <- getBy404 $ UniquePerformanceWeekIdPlayerId weekId playerId
    mRecPerformance <- mapM (getBy404 . UniquePerformanceWeekIdPlayerId weekId) recPlayerId

    let (seasonId, action) = (weekSeasonId week, eventAction event)
    Entity _ scoringSettings <- getBy404 $ UniqueScoringSettingsSeasonIdAction seasonId action

    let cumulative = performanceCappedCumulativePoints performance
        cumulativeRec = fromMaybe 0 $ map performanceCappedCumulativePoints $ map entityVal mRecPerformance
        (aPoints, wPoints, aPointsRec, wPointsRec) = calculatePoints scoringSettings cumulative cumulativeRec

    return (aPoints, wPoints, aPointsRec, wPointsRec, playerId, recPlayerId)

calculatePoints :: ScoringSettings -> Rational -> Rational -> (Rational, Rational, Rational, Rational)
calculatePoints scoringSettings cumulativePoints cumulativePointsReceiving =
    if scoringSettingsIsUsed scoringSettings then
        let points    = toRational $ scoringSettingsPoints scoringSettings
            weight    = (toRational $ scoringSettingsWeight scoringSettings) / 100
            pointsRec = toRational $ scoringSettingsPointsReceiving scoringSettings
            weightRec = (toRational $ scoringSettingsWeightReceiving scoringSettings) / 100
            -- Shorten the names so the next few lines aren't super long
            cumul     = cumulativePoints
            cumulRec  = cumulativePointsReceiving
        in  if isMultiCharacter $ scoringSettingsAction scoringSettings
                -- multi-character actions
                then if weightRec >= 0
                    -- if receiving weight is positive, weight the cumulative points of the opposite player
                    then (points, weight * cumulRec, pointsRec, weightRec * cumul)
                    -- if receiving weight is negative, weight the cumulative points of the receiving player only
                    else (points, weight * cumulRec, pointsRec, weightRec * cumulRec)
                -- single character actions weight the cumulative points of that player
                else (points, weight * cumul, 0, 0)
    else (0, 0, 0, 0)

updatePointsFromPlay :: Play -> Bool -> Handler ()
updatePointsFromPlay play negatePoints = do
    let weekId = playWeekId play
    -- add points to performance and game for player
    let points = negatePointsIfRelevant $ playPoints play
    updatePointsFromPlayer weekId (playPlayerId play) points
    case playReceivingPlayerId play of
        Nothing -> return ()
        -- add points to performance and game for receiving player if relevant
        Just receivingPlayerId -> do
            let recPoints = negatePointsIfRelevant $ playReceivingPoints play
            updatePointsFromPlayer weekId receivingPlayerId recPoints
    where negatePointsIfRelevant points =
              if negatePoints then negate points else points

updatePointsFromPlayer :: WeekId -> PlayerId -> Rational -> Handler ()
updatePointsFromPlayer weekId playerId points = do
    player <- runDB $ get404 playerId
    if points == 0 || not (playerIsPlayable player) then return () else do
        Entity performanceId performance <- runDB $ getBy404 $ UniquePerformanceWeekIdPlayerId weekId playerId
        addToPerformancePoints performanceId points
        addToGamePoints performance weekId points

addToPerformancePoints :: PerformanceId -> Rational -> Handler ()
addToPerformancePoints performanceId points = do
    now <- liftIO getCurrentTime
    runDB $ update performanceId [ PerformancePoints    +=. points
                                 , PerformanceUpdatedAt  =. now ]

addToGamePoints :: Performance -> WeekId -> Rational -> Handler ()
addToGamePoints performance weekId points =
    case (performanceTeamId performance, performanceIsStarter performance) of
        -- if the player is a starter on a team, add points to this team's game
        -- for this week, and to the team's season total
        (Just teamId, True) -> runDB $ do
            Entity gameId _ <- getBy404 $ UniqueGameWeekIdTeamId weekId teamId
            now <- liftIO getCurrentTime
            update gameId [GamePoints +=. points, GameUpdatedAt =. now]
        (_, _) -> return ()

calculatePointsThisSeason :: Week -> UserId -> Handler ()
calculatePointsThisSeason week userId = do
    let seasonId = weekSeasonId week
    teamSeasons <- runDB $ selectList [TeamSeasonSeasonId ==. seasonId] []
    forM_ teamSeasons $ calculateTeamSeasonPoints userId
    playerSeasons <- runDB $ selectList [PlayerSeasonSeasonId ==. seasonId] []
    forM_ playerSeasons $ calculatePlayerSeasonPoints userId

calculateTeamSeasonPoints :: UserId -> Entity TeamSeason -> Handler ()
calculateTeamSeasonPoints userId (Entity teamSeasonId teamSeason) = do
    let seasonId = teamSeasonSeasonId teamSeason
    games <- getGamesForTeam seasonId $ teamSeasonTeamId teamSeason
    let (postSeason, regularSeason) = partition isPostSeason games
    now <- liftIO getCurrentTime
    runDB $ update teamSeasonId [ TeamSeasonTotalPoints =. totalPoints games
                                , TeamSeasonRegularSeasonPoints =. totalPoints regularSeason
                                , TeamSeasonPostSeasonPoints =. totalPoints postSeason
                                , TeamSeasonUpdatedBy =. userId
                                , TeamSeasonUpdatedAt =. now ]
    where isPostSeason (_, Entity _ week) = weekIsPostSeason week
          totalPoints gameWeekList = sum $ map (gamePoints . entityVal . fst) gameWeekList

calculatePlayerSeasonPoints :: UserId -> Entity PlayerSeason -> Handler ()
calculatePlayerSeasonPoints userId (Entity playerSeasonId playerSeason) = do
    let seasonId = playerSeasonSeasonId playerSeason
    performances <- getPerformancesForPlayer seasonId $ playerSeasonPlayerId playerSeason
    let (postSeason, regularSeason) = break isPostSeason performances
        pointsThisSeason = totalPoints performances
    now <- liftIO getCurrentTime
    runDB $ update playerSeasonId [ PlayerSeasonTotalPoints =. pointsThisSeason
                                  , PlayerSeasonPostSeasonPoints =. totalPoints postSeason
                                  , PlayerSeasonRegularSeasonPoints =. totalPoints regularSeason
                                  , PlayerSeasonUpdatedBy =. userId
                                  , PlayerSeasonUpdatedAt =. now ]
    where isPostSeason (_, Entity _ week) = weekIsPostSeason week
          totalPoints performanceWeekList = sum $ map (performancePoints . entityVal . fst) performanceWeekList

calculateNextWeekCumulativePoints :: Week -> Handler ()
calculateNextWeekCumulativePoints week = do
    let (nextWeekNumber, seasonId) = (weekNumber week + 1, weekSeasonId week)
    nextWeek <- runDB $ getBy $ UniqueWeekSeasonIdNumber seasonId nextWeekNumber
    for_ nextWeek calculateWeekCumulativePoints

calculateWeekCumulativePoints :: Entity Week -> Handler ()
calculateWeekCumulativePoints (Entity weekId week) = do
    episode <- runDB $ get404 $ weekEpisodeId week
    previousWeeks <- getPreviousWeeks week episode
    performances <- runDB $ selectList [PerformanceWeekId ==. weekId] []
    forM_ performances $ updateCumulativePoints $ map entityKey previousWeeks

updateCumulativePoints :: [WeekId] -> Entity Performance -> Handler ()
updateCumulativePoints previousWeekIds (Entity performanceId performance) = do
    let playerId = performancePlayerId performance
    (cumulativePoints, cappedCumulativePoints) <- calculateCumulativePoints previousWeekIds playerId
    now <- liftIO getCurrentTime
    runDB $ update performanceId [ PerformanceCumulativePoints =. cumulativePoints
                                 , PerformanceCappedCumulativePoints =. cappedCumulativePoints
                                 , PerformanceUpdatedAt =. now ]

calculateCumulativePoints :: [WeekId] -> PlayerId -> Handler (Rational, Rational)
calculateCumulativePoints previousWeekIds playerId = do
    performances <- runDB $ selectList [ PerformancePlayerId ==. playerId
                                       , PerformanceWeekId <-. previousWeekIds
                                       ] []
    let cumulativePoints = totalPoints performances
        cappedCumulative = max 0 cumulativePoints
    return (cumulativePoints, cappedCumulative)
    where totalPoints performanceList = sum $ map (performancePoints . entityVal) performanceList


-------------
-- Helpers --
-------------
userName :: Entity User -> Text
userName (Entity userId user) =
    case (userFirstName user, userLastName user) of
        (Nothing, Nothing) -> "User #" ++ (pack $ show $ S.fromSqlKey userId)
        (firstN,  lastN)   -> fromMaybe "" firstN ++ fromMaybe "" lastN

isLastRegularSeasonWeek :: Week -> Handler Bool
isLastRegularSeasonWeek week = do
    let seasonId = weekSeasonId week
    Entity _ generalSettings <- runDB $ getBy404 $ UniqueGeneralSettingsSeasonId seasonId
    return $ weekNumber week == generalSettingsRegularSeasonLength generalSettings

