module Handler.Score where

import Import

import qualified Database.Persist.Sql as S (fromSqlKey)
import           Data.Maybe                (fromJust)

import qualified Database.Esqueleto as E
import           Database.Esqueleto ((^.))

-------------
-- Queries --
-------------
getGamesForTeam :: SeasonId -> TeamId -> Handler [(Entity Game, Entity Week)]
getGamesForTeam seasonId teamId = runDB
    $ E.select
    $ E.from $ \(game `E.InnerJoin` week) -> do
        E.on $ game ^. GameWeekId E.==. week ^. WeekId
        E.where_ $ game ^. GameTeamId E.==. E.val teamId
            E.&&. week ^. WeekSeasonId E.==. E.val (Just seasonId)
        E.orderBy [E.asc (week ^. WeekNumber)]
        return (game, week)

getPerformancesForPlayer :: SeasonId -> PlayerId -> Handler [(Entity Performance, Entity Week)]
getPerformancesForPlayer seasonId playerId = runDB
    $ E.select
    $ E.from $ \(performance `E.InnerJoin` week) -> do
        E.on $ performance ^. PerformanceWeekId E.==. week ^. WeekId
        E.where_ $ performance ^. PerformancePlayerId E.==. E.val playerId
            E.&&. week ^. WeekSeasonId E.==. E.val (Just seasonId)
        E.orderBy [E.asc (week ^. WeekNumber)]
        return (performance, week)


--------------------
-- Finalize Weeks --
--------------------
finalizeWeek :: EpisodeId -> UserId -> UTCTime -> Entity League -> Handler ()
finalizeWeek episodeId userId now (Entity leagueId _league) = do
    Entity weekId week <- runDB $ getBy404 $ UniqueWeekLeagueIdEpisodeId leagueId episodeId
    calculatePointsThisSeason week userId now
    calculateNextWeekCumulativePoints week now
    determineWaiverOrder week userId now
    moveSeasonToPostSeason week userId now
    markWeekAsScored (Entity weekId week) now
    -- TODO - Implement the below function and un-comment the below line
    -- emailTeamOwners episode -- Email only if league draft is complete

markWeekAsScored :: Entity Week -> UTCTime -> Handler ()
markWeekAsScored (Entity weekId week) now = if weekIsScored week then return () else
    runDB $ update weekId [WeekIsScored =. True, WeekUpdatedAt =. now]

determineWaiverOrder :: Week -> UserId -> UTCTime -> Handler ()
determineWaiverOrder week userId now = runDB $ do
    -- order teams by points (in regular or postseason) and make that the waiver order
    let seasonId = fromJust $ weekSeasonId week
        conditions = if weekIsPostSeason week
                         then [ Asc  TeamSeasonPostSeasonStatus
                              , Asc  TeamSeasonPostSeasonPoints
                              , Asc  TeamSeasonRegularSeasonPoints]
                         else [ Asc  TeamSeasonRegularSeasonPoints
                              , Desc TeamSeasonDraftOrder]
    teamSeasonIds <- selectKeysList [TeamSeasonSeasonId ==. seasonId] conditions
    mapM_ (updateWaiverOrder userId now) $ rank teamSeasonIds

updateWaiverOrder :: UserId -> UTCTime -> (Int, TeamSeasonId) -> ReaderT SqlBackend Handler ()
updateWaiverOrder userId now (waiverOrder, teamSeasonId) = do
    update teamSeasonId [ TeamSeasonWaiverOrder =. waiverOrder
                        , TeamSeasonUpdatedBy =. userId
                        , TeamSeasonUpdatedAt =. now ]

moveSeasonToPostSeason :: Week -> UserId -> UTCTime -> Handler ()
moveSeasonToPostSeason week userId now = do
    -- move league to postseason if this week is the last week of regular season
    lastRegularSeasonWeek <- isLastRegularSeasonWeek week
    if not lastRegularSeasonWeek then return () else runDB $ do
        let (leagueId, seasonId) = (weekLeagueId week, weekSeasonId week)
        -- first, split teams into playoffs and consolation
        teamSeasonIds <- selectKeysList [TeamSeasonSeasonId ==. fromJust seasonId]
                                        [Desc TeamSeasonRegularSeasonPoints]
        -- TODO - get rid of the below two lines
        maybeGeneralSettings <- selectFirst [ GeneralSettingsLeagueId ==. leagueId
                                            , GeneralSettingsSeasonId ==. seasonId
                                            ] []
        let Entity _ generalSettings = fromJust maybeGeneralSettings
        -- TODO - use the below line once the unique constraint can be added
        -- Entity _ generalSettings <- runDB $ getBy404 $ UniqueGeneralSettingsSeasonId seasonId 
        let teamsInPlayoffs = generalSettingsNumberOfTeamsInPlayoffs generalSettings
            (playoffTSIds, consolationTSIds) = splitAt teamsInPlayoffs teamSeasonIds
        mapM_ (moveTeamSeasonToPostSeason Playoff userId now) playoffTSIds
        mapM_ (moveTeamSeasonToPostSeason Consolation userId now) consolationTSIds

        -- then, set league to being in postseason
        update (fromJust seasonId) [ SeasonIsInPostSeason =. True
                        , SeasonUpdatedBy =. userId
                        , SeasonUpdatedAt =. now ]

moveTeamSeasonToPostSeason :: PostSeasonStatus -> UserId -> UTCTime ->
                              TeamSeasonId -> ReaderT SqlBackend Handler ()
moveTeamSeasonToPostSeason postSeasonStatus userId now teamSeasonId =
    update teamSeasonId [ TeamSeasonPostSeasonStatus =. postSeasonStatus
                        , TeamSeasonUpdatedBy =. userId
                        , TeamSeasonUpdatedAt =. now ]


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

    -- TODO - get rid of the below two lines
    maybeScoringSettings <- selectFirst [ ScoringSettingsLeagueId ==. leagueId
                                        , ScoringSettingsSeasonId ==. weekSeasonId week
                                        , ScoringSettingsAction   ==. eventAction event
                                        ] []
    let Entity _ scoringSettings = fromJust maybeScoringSettings
    -- TODO - use the below line once the unique constraint can be added
    -- Entity _ scoringSettings <- getBy404 $ UniqueScoringSettingsSeasonIdAction (weekSeasonId week) $ eventAction event

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
    let seasonId = fromJust $ weekSeasonId week
    teamSeasons <- runDB $ selectList [TeamSeasonSeasonId ==. seasonId] []
    forM_ teamSeasons $ calculateTeamSeasonPoints userId now
    playerSeasons <- runDB $ selectList [PlayerSeasonSeasonId ==. seasonId] []
    forM_ playerSeasons $ calculatePlayerSeasonPoints userId now

calculateTeamSeasonPoints :: UserId -> UTCTime -> Entity TeamSeason -> Handler ()
calculateTeamSeasonPoints userId now (Entity teamSeasonId teamSeason) = do
    let seasonId = teamSeasonSeasonId teamSeason
    games <- getGamesForTeam seasonId $ teamSeasonTeamId teamSeason
    let (postSeason, regularSeason) = partition isPostSeason games
    runDB $ update teamSeasonId [ TeamSeasonTotalPoints =. totalPoints games
                                , TeamSeasonRegularSeasonPoints =. totalPoints regularSeason
                                , TeamSeasonPostSeasonPoints =. totalPoints postSeason
                                , TeamSeasonUpdatedBy =. userId
                                , TeamSeasonUpdatedAt =. now ]
    where isPostSeason (_, Entity _ week) = weekIsPostSeason week
          totalPoints gameWeekList = sum $ map (gamePoints . entityVal . fst) gameWeekList

calculatePlayerSeasonPoints :: UserId -> UTCTime -> Entity PlayerSeason -> Handler ()
calculatePlayerSeasonPoints userId now (Entity playerSeasonId playerSeason) = do
    let seasonId = playerSeasonSeasonId playerSeason
    performances <- getPerformancesForPlayer seasonId $ playerSeasonPlayerId playerSeason
    let (postSeason, regularSeason) = break isPostSeason performances
        pointsThisSeason = totalPoints performances
    runDB $ update playerSeasonId [ PlayerSeasonTotalPoints =. pointsThisSeason
                                  , PlayerSeasonPostSeasonPoints =. totalPoints postSeason
                                  , PlayerSeasonRegularSeasonPoints =. totalPoints regularSeason
                                  , PlayerSeasonUpdatedBy =. userId
                                  , PlayerSeasonUpdatedAt =. now ]
    where isPostSeason (_, Entity _ week) = weekIsPostSeason week
          totalPoints performanceWeekList = sum $ map (performancePoints . entityVal . fst) performanceWeekList

calculateNextWeekCumulativePoints :: Week -> UTCTime -> Handler ()
calculateNextWeekCumulativePoints week now = do
    let (nextWeekNumber, seasonId) = (weekNumber week + 1, weekSeasonId week)
    -- TODO remove the below line
    nextWeek <- runDB $ selectFirst [WeekSeasonId ==. seasonId, WeekNumber ==. nextWeekNumber] []
    -- TODO - use the below line once the unique constraint can be added
    -- nextWeek <- runDB $ getBy $ UniqueWeekSeasonIdNumber seasonId nextWeekNumber
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
    let (leagueId, seasonId) = (weekLeagueId week, weekSeasonId week)
    -- TODO - get rid of the below two lines
    maybeGeneralSettings <- runDB $ selectFirst [ GeneralSettingsLeagueId ==. leagueId
                                                , GeneralSettingsSeasonId ==. seasonId
                                                ] []
    let Entity _ generalSettings = fromJust maybeGeneralSettings
    -- TODO - use the below line once the unique constraint can be added
    -- Entity _ generalSettings <- runDB $ getBy404 $ UniqueGeneralSettingsSeasonId seasonId 
    return $ weekNumber week == generalSettingsRegularSeasonLength generalSettings

