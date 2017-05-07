module Handler.Score where

import Import

import qualified Database.Persist.Sql as S (fromSqlKey)

--------------------
-- Finalize Weeks --
--------------------
finalizeWeek :: EpisodeId -> UserId -> UTCTime -> Entity League -> Handler ()
finalizeWeek episodeId userId now (Entity leagueId _league) = do
    Entity weekId week <- runDB $ getBy404 $ UniqueWeekLeagueIdEpisodeId leagueId episodeId
    calculatePointsThisSeason week userId now
    calculateNextWeekCumulativePoints week now
    determineWaiverOrder week userId now
    moveLeagueToPostSeason week userId now
    markWeekAsScored (Entity weekId week) now
    -- TODO - Implement the below function and un-comment the below line
    -- emailTeamOwners episode -- Email only if league draft is complete

markWeekAsScored :: Entity Week -> UTCTime -> Handler ()
markWeekAsScored (Entity weekId week) now = if weekIsScored week then return () else
    runDB $ update weekId [WeekIsScored =. True, WeekUpdatedAt =. now]

determineWaiverOrder :: Week -> UserId -> UTCTime -> Handler ()
determineWaiverOrder week userId now = do
    -- order teams by points (in regular or postseason) and make that the waiver order
    teams <- runDB $ selectList [TeamLeagueId ==. weekLeagueId week]
                                [if not (weekIsPostSeason week)
                                     then Asc TeamPointsThisRegularSeason
                                     else Asc TeamPointsThisPostSeason
                                        , Asc TeamPointsThisRegularSeason
                                ]
    mapM_ (updateWaiverOrder userId now) $ rank teams

updateWaiverOrder :: UserId -> UTCTime -> (Int, Entity Team) -> Handler ()
updateWaiverOrder userId now (waiverOrder, Entity teamId _) = do
    runDB $ update teamId [ TeamWaiverOrder =. waiverOrder
                          , TeamUpdatedBy =. userId
                          , TeamUpdatedAt =. now ]

moveLeagueToPostSeason :: Week -> UserId -> UTCTime -> Handler ()
moveLeagueToPostSeason week userId now = do
    -- move league to postseason if this week is the last week of regular season
    lastRegularSeasonWeek <- isLastRegularSeasonWeek week
    if not lastRegularSeasonWeek then return () else runDB $ do
        let leagueId = weekLeagueId week
        -- first, split teams into playoffs and consolation
        teamIds <- selectKeysList [TeamLeagueId ==. leagueId]
                                  [Desc TeamPointsThisRegularSeason]
        Entity _ generalSettings <- getBy404 $ UniqueGeneralSettingsLeagueId leagueId
        let teamsInPlayoffs = generalSettingsNumberOfTeamsInPlayoffs generalSettings
            (playoffTeamIds, consolationTeamIds) = splitAt teamsInPlayoffs teamIds
        mapM_ (moveTeamToPostSeason Playoff userId now) playoffTeamIds
        mapM_ (moveTeamToPostSeason Consolation userId now) consolationTeamIds

        -- then, set league to being in postseason
        update leagueId [ LeagueIsInPostSeason =. True
                        , LeagueUpdatedBy =. userId
                        , LeagueUpdatedAt =. now ]

moveTeamToPostSeason :: PostSeasonStatus -> UserId -> UTCTime -> TeamId -> ReaderT SqlBackend Handler ()
moveTeamToPostSeason postSeasonStatus userId now teamId =
    update teamId [ TeamPostSeasonStatus =. postSeasonStatus
                  , TeamUpdatedBy =. userId
                  , TeamUpdatedAt =. now ]


------------------
-- Upsert Plays --
------------------
updatePlayNotes :: Entity Event -> Handler ()
updatePlayNotes (Entity eventId event) = runDB $ do
    now <- liftIO getCurrentTime
    playIds <- selectKeysList [PlayEventId ==. eventId] []
    forM_ playIds (\playId -> update playId [PlayNote =. eventNote event, PlayUpdatedAt =. now])

upsertPlays :: Entity Event -> Handler ()
upsertPlays eventEntity = do
    leagueIds <- runDB $ selectKeysList [LeagueIsActive ==. True] [Asc LeagueId]
    mapM_ (upsertPlay eventEntity) leagueIds

upsertPlay :: Entity Event -> LeagueId -> Handler ()
upsertPlay (Entity eventId event) leagueId = do
    let episodeId = eventEpisodeId event
    Entity weekId _ <- runDB $ getBy404 $ UniqueWeekLeagueIdEpisodeId leagueId episodeId
    maybePlayEntity <- runDB $ getBy $ UniquePlayWeekIdEventId weekId eventId
    case maybePlayEntity of
        Nothing -> createPlay leagueId weekId $ Entity eventId event
        Just playEntity -> updatePlay event playEntity

createPlay :: LeagueId -> WeekId -> Entity Event -> Handler ()
createPlay leagueId weekId (Entity eventId event) = do
    (aPoints, wPoints, aRecPnts, wRecPnts, playerId, mRecPlayerId) <- calculatePointsAndPlayers leagueId weekId event
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

updatePlay :: Event -> Entity Play -> Handler ()
updatePlay event (Entity playId play) = do
    let (leagueId, weekId) = (playLeagueId play, playWeekId play)
    (aPoints, wPoints, aRecPnts, wRecPnts, playerId, mRecPlayerId) <- calculatePointsAndPlayers leagueId weekId event
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


----------------------
-- Calculate Points --
----------------------
calculatePointsAndPlayers :: LeagueId -> WeekId -> Event ->
                             Handler (Rational, Rational, Rational, Rational, PlayerId, Maybe PlayerId)
calculatePointsAndPlayers leagueId weekId event = runDB $ do
    let (charId, recCharId) = (eventCharacterId event, eventReceivingCharacterId event)
    Entity playerId _ <- getBy404 $ UniquePlayerLeagueIdCharacterId leagueId charId
    mRecPlayer <- mapM (getBy404 . UniquePlayerLeagueIdCharacterId leagueId) recCharId

    let recPlayerId = map entityKey mRecPlayer
    Entity _ performance <- getBy404 $ UniquePerformanceWeekIdPlayerId weekId playerId
    mRecPerformance <- mapM (getBy404 . UniquePerformanceWeekIdPlayerId weekId) recPlayerId

    Entity _ scoringSettings <- getBy404 $ UniqueScoringSettingsLeagueIdAction leagueId $ eventAction event

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
        now <- liftIO getCurrentTime
        Entity performanceId performance <- runDB $ getBy404 $ UniquePerformanceWeekIdPlayerId weekId playerId
        addToPerformancePoints performanceId points now
        addToGamePoints performance weekId points now

addToPerformancePoints :: PerformanceId -> Rational -> UTCTime -> Handler ()
addToPerformancePoints performanceId points now =
    runDB $ update performanceId [ PerformancePoints    +=. points
                                 , PerformanceUpdatedAt  =. now ]

addToGamePoints :: Performance -> WeekId -> Rational -> UTCTime -> Handler ()
addToGamePoints performance weekId points now =
    case (performanceTeamId performance, performanceIsStarter performance) of
        -- if the player is a starter on a team, add points to this team's game
        -- for this week, and to the team's season total
        (Just teamId, True) -> runDB $ do
            Entity gameId _ <- getBy404 $ UniqueGameWeekIdTeamId weekId teamId
            update gameId [GamePoints +=. points, GameUpdatedAt =. now]
        (_, _) -> return ()

calculatePointsThisSeason :: Week -> UserId -> UTCTime -> Handler ()
calculatePointsThisSeason week userId now = do
    let leagueId = weekLeagueId week
    postSeasonWeekIds <- runDB $ selectKeysList [ WeekLeagueId ==. leagueId
                                                , WeekIsPostSeason ==. True
                                                ] [Asc WeekNumber]
    teamIds <- runDB $ selectKeysList [TeamLeagueId ==. leagueId] []
    forM_ teamIds $ calculateTeamPointsThisSeason postSeasonWeekIds userId now
    playerIds <- runDB $ selectKeysList [PlayerLeagueId ==. leagueId] []
    forM_ playerIds $ calculatePlayerPointsThisSeason postSeasonWeekIds userId now

calculateTeamPointsThisSeason :: [WeekId] -> UserId -> UTCTime -> TeamId -> Handler ()
calculateTeamPointsThisSeason postSeasonWeekIds userId now teamId = runDB $ do
    games <- selectList [GameTeamId ==. teamId] []
    let (postSeason, regularSeason) = break isPostSeason games
    update teamId [ TeamPointsThisSeason =. totalPoints games
                  , TeamPointsThisRegularSeason =. totalPoints regularSeason
                  , TeamPointsThisPostSeason =. totalPoints postSeason
                  , TeamUpdatedBy =. userId
                  , TeamUpdatedAt =. now ]
    where isPostSeason game = elem (gameWeekId $ entityVal game) postSeasonWeekIds
          totalPoints gameList = sum $ map (gamePoints . entityVal) gameList

calculatePlayerPointsThisSeason :: [WeekId] -> UserId -> UTCTime -> PlayerId -> Handler ()
calculatePlayerPointsThisSeason postSeasonWeekIds userId now playerId = runDB $ do
    performances <- selectList [PerformancePlayerId ==. playerId] []
    let (postSeason, regularSeason) = break isPostSeason performances
        pointsThisSeason = totalPoints performances
    update playerId [ PlayerPointsThisSeason =. pointsThisSeason
                    , PlayerPointsThisPostSeason =. totalPoints postSeason
                    , PlayerPointsThisRegularSeason =. totalPoints regularSeason
                    , PlayerUpdatedBy =. userId
                    , PlayerUpdatedAt =. now ]
    where isPostSeason performance = elem (performanceWeekId $ entityVal performance) postSeasonWeekIds
          totalPoints performanceList = sum $ map (performancePoints . entityVal) performanceList

calculateNextWeekCumulativePoints :: Week -> UTCTime -> Handler ()
calculateNextWeekCumulativePoints week now = do
    let (nextWeekNumber, leagueId) = (weekNumber week + 1, weekLeagueId week)
    nextWeek <- runDB $ getBy $ UniqueWeekLeagueIdWeekNumber leagueId nextWeekNumber
    for_ nextWeek $ calculateWeekCumulativePoints now

calculateWeekCumulativePoints :: UTCTime -> Entity Week -> Handler ()
calculateWeekCumulativePoints now (Entity weekId week) = do
    -- TODO - when seasons are added, somehow pull the last 7 weeks,
    -- and make 7 configurable somehow/somewhere
    previousWeekIds <- runDB $ selectKeysList [WeekNumber <. weekNumber week] []
    performances <- runDB $ selectList [PerformanceWeekId ==. weekId] []
    forM_ performances $ updateCumulativePoints previousWeekIds now

updateCumulativePoints :: [WeekId] -> UTCTime -> Entity Performance -> Handler ()
updateCumulativePoints previousWeekIds now (Entity performanceId performance) = do
    let playerId = performancePlayerId performance
    (cumulativePoints, cappedCumulativePoints) <- calculateCumulativePoints previousWeekIds playerId
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
    let leagueId = weekLeagueId week
    Entity _ generalSettings <- runDB $ getBy404 $ UniqueGeneralSettingsLeagueId leagueId
    return $ weekNumber week == generalSettingsRegularSeasonLength generalSettings

