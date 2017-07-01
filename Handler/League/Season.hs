module Handler.League.Season where

import Import

import Handler.Score              (calculateCumulativePoints, createPlay, finalizeWeek, getPreviousWeeks)
import Handler.League.Transaction (cancelAllTrades, cancelAllTransactionRequests)

import Data.Maybe                   (fromJust)
import Data.Random.List
import Data.Random.RVar
import Data.Random.Source.DevRandom
import Network.Mail.Mime
import System.Random                (newStdGen)

-------------
-- Seasons --
-------------
updateLeagueSeasonsIfRelevent :: Series -> Entity Series -> Handler ()
updateLeagueSeasonsIfRelevent oldSeries (Entity seriesId series) =
    if seriesYear series == seriesYear oldSeries then return () else runDB $
        updateWhere [ SeasonSeriesId ==. seriesId ]
                    [ SeasonYear =. seriesYear series
                    , SeasonUpdatedBy =. seriesUpdatedBy series
                    , SeasonUpdatedAt =. seriesUpdatedAt series
                    ]

createLeagueSeasons :: Entity Series -> Handler ()
createLeagueSeasons seriesEntity = do
    leagues <- runDB $ selectList [] [Asc LeagueId]
    let userId = seriesCreatedBy $ entityVal seriesEntity
    forM_ leagues $ createLeagueSeason userId seriesEntity

createLeagueSeason :: UserId -> Entity Series -> Entity League -> Handler ()
createLeagueSeason userId (Entity seriesId series) (Entity leagueId league) = do
    maybeSeason <- runDB $ getBy $ UniqueSeasonLeagueIdSeriesId leagueId seriesId
    if isJust maybeSeason then return () else do
        seasonEntity <- runDB $ do
            let leagueEntity = Entity leagueId league
            Entity seasonId season <- createSeason userId (Entity seriesId series) leagueId
            inactivateOtherSeasons userId seasonId leagueId
            createTeamInfo userId seasonId leagueEntity

            maybePreviousSeasonId <- selectPreviousSeasonId leagueId series
            createGeneralSettings userId seasonId leagueEntity maybePreviousSeasonId
            mapM_ (createScoringSettingsRow userId seasonId leagueEntity maybePreviousSeasonId) allActions
            return $ Entity seasonId season

        backgroundHandler $ do
            runDB $ createPlayerInfo userId (entityKey seasonEntity) leagueId
            episodes <- runDB $ selectList [ EpisodeSeriesId ==. seriesId
                                           , EpisodeStatus !=. YetToAir
                                           ] [Asc EpisodeNumber]
            mapM_ (backfillWeekData userId seasonEntity) episodes

inactivateOtherSeasons :: UserId -> SeasonId -> LeagueId -> ReaderT SqlBackend Handler ()
inactivateOtherSeasons userId seasonId leagueId = do
    now <- liftIO getCurrentTime
    updateWhere [ SeasonId !=. seasonId, SeasonLeagueId ==. leagueId ]
                [ SeasonIsActive =. False
                , SeasonUpdatedBy =. userId
                , SeasonUpdatedAt =. now
                ]

createSeason :: UserId -> Entity Series -> LeagueId -> ReaderT SqlBackend Handler (Entity Season)
createSeason userId (Entity seriesId series) leagueId = do
    now <- liftIO getCurrentTime
    let season = Season { seasonLeagueId = leagueId
                        , seasonYear = seriesYear series
                        , seasonSeriesId = seriesId
                        , seasonIsActive = True
                        , seasonIsDraftComplete = False
                        , seasonIsInPostSeason  = False
                        , seasonIsAfterTradeDeadline = False
                        , seasonIsSeasonComplete = False
                        , seasonCreatedBy = userId
                        , seasonCreatedAt = now
                        , seasonUpdatedBy = userId
                        , seasonUpdatedAt = now
                        , seasonDraftCompletedAt = Nothing
                        }
    seasonId <- insert season
    return $ Entity seasonId season

selectPreviousSeasonId :: LeagueId -> Series -> ReaderT SqlBackend Handler (Maybe SeasonId)
selectPreviousSeasonId leagueId series = do
    previousSeries <- getBy $ UniqueSeriesNumber $ seriesNumber series - 1
    case previousSeries of
        Nothing -> return Nothing
        Just (Entity previousSeriesId _) -> do
            mPrevSeason <- getBy $ UniqueSeasonLeagueIdSeriesId leagueId previousSeriesId
            return $ map entityKey mPrevSeason

-----------
-- Teams --
-----------
createTeamInfo :: UserId -> SeasonId -> Entity League -> ReaderT SqlBackend Handler ()
createTeamInfo userId seasonId leagueEntity = do
    teams <- selectOrCreateTeams userId leagueEntity
    forM_ teams $ createTeamSeason userId (entityKey leagueEntity) seasonId

selectOrCreateTeams :: UserId -> Entity League -> ReaderT SqlBackend Handler [Entity Team]
selectOrCreateTeams userId (Entity leagueId league) = do
    teams <- selectList [TeamLeagueId ==. leagueId] []
    if length teams > 0 then return teams else do
        let teamNumbers = [1..(leagueTeamsCount league)]
        draftOrder <- liftIO (runRVar (shuffle teamNumbers) DevRandom :: IO [Int])
        mapM (createTeam userId $ Entity leagueId league) $ zip teamNumbers draftOrder

createTeam :: UserId -> Entity League -> (Int, Int) -> ReaderT SqlBackend Handler (Entity Team)
createTeam userId (Entity leagueId league) (number, draftOrder) = do
    now <- liftIO getCurrentTime
    stdgen <- liftIO newStdGen
    let (name, abbrev, owner, email) = teamTextAttributes number
        (maybeTeamOwnerId, maybeConfirmedAt) = teamNonTextAttributes league number
        verificationKey = pack $ fst $ randomString 24 stdgen
        team = Team { teamLeagueId         = leagueId
                    , teamNumber           = number
                    , teamName             = name
                    , teamAbbreviation     = abbrev
                    , teamOwnerId          = maybeTeamOwnerId
                    , teamOwnerName        = owner
                    , teamOwnerEmail       = email
                    , teamIsConfirmed      = isJust maybeConfirmedAt
                    , teamPlayersCount     = 0
                    , teamStartersCount    = 0
                    , teamDraftOrder       = draftOrder
                    , teamWaiverOrder      = number
                    , teamVerificationKey  = verificationKey
                    , teamPointsThisSeason = 0
                    , teamPointsThisRegularSeason = 0
                    , teamPointsThisPostSeason = 0
                    , teamPostSeasonStatus = Regular
                    , teamCreatedBy        = userId
                    , teamCreatedAt        = now
                    , teamUpdatedBy        = userId
                    , teamUpdatedAt        = now
                    , teamConfirmedBy      = maybeTeamOwnerId
                    , teamConfirmedAt      = maybeConfirmedAt
                    , teamJoinEmailResentBy = Nothing
                    , teamJoinEmailResentAt = Nothing
                    }
    teamId <- insert team
    return $ Entity teamId team

createTeamSeason :: UserId -> LeagueId -> SeasonId -> Entity Team -> ReaderT SqlBackend Handler ()
createTeamSeason userId leagueId seasonId (Entity teamId team) = do
    now <- liftIO getCurrentTime
    insert_ $ TeamSeason { teamSeasonLeagueId = leagueId
                         , teamSeasonTeamId = teamId
                         , teamSeasonSeasonId = seasonId
                         , teamSeasonPlayersCount = 0
                         , teamSeasonStartersCount = 0
                         , teamSeasonDraftOrder = teamDraftOrder team
                         , teamSeasonWaiverOrder = 0
                         , teamSeasonTotalPoints = 0
                         , teamSeasonRegularSeasonPoints = 0
                         , teamSeasonPostSeasonPoints = 0
                         , teamSeasonPostSeasonStatus = Regular
                         , teamSeasonCreatedBy = userId
                         , teamSeasonCreatedAt = now
                         , teamSeasonUpdatedBy = userId
                         , teamSeasonUpdatedAt = now
                         }

teamTextAttributes :: Int -> (Text, Text, Text, Text)
teamTextAttributes 1 = ("My House Name", "N1", "My Name", "My Email Address")
teamTextAttributes n =
    let s = (pack . show) n
    in  ("Number " ++ s, "N" ++ s, "Owner " ++ s, "Owner " ++ s ++ "'s email")

teamNonTextAttributes :: League -> Int -> (Maybe UserId, Maybe UTCTime)
teamNonTextAttributes league 1 =
    (Just $ leagueCreatedBy league, Just $ leagueCreatedAt league)
teamNonTextAttributes _ _ = (Nothing, Nothing)


-------------
-- Players --
-------------
createPlayerInfo :: UserId -> SeasonId -> LeagueId -> ReaderT SqlBackend Handler ()
createPlayerInfo userId seasonId leagueId = do
    playerIds <- selectOrCreatePlayerIds userId leagueId
    forM_ playerIds $ createPlayerSeason userId leagueId seasonId

selectOrCreatePlayerIds :: UserId -> LeagueId -> ReaderT SqlBackend Handler [PlayerId]
selectOrCreatePlayerIds userId leagueId = do
    playerIds <- selectKeysList [PlayerLeagueId ==. leagueId] []
    if length playerIds > 0 then return playerIds else do
        characters <- selectList [] [Asc CharacterName]
        mapM (createPlayer userId leagueId) characters

createPlayer :: UserId -> LeagueId -> Entity Character ->
                ReaderT SqlBackend Handler PlayerId
createPlayer userId leagueId (Entity characterId character) = do
    now <- liftIO getCurrentTime
    insert $ Player { playerLeagueId         = leagueId
                    , playerCharacterId      = characterId
                    , playerTeamId           = Nothing
                    , playerIsStarter        = False
                    , playerPointsThisSeason = 0
                    , playerPointsThisRegularSeason = 0
                    , playerPointsThisPostSeason = 0
                    , playerIsPlayable       = characterIsPlayable character
                    , playerCreatedBy        = userId
                    , playerCreatedAt        = now
                    , playerUpdatedBy        = userId
                    , playerUpdatedAt        = now
                    }

createPlayerSeason :: UserId -> LeagueId -> SeasonId -> PlayerId -> ReaderT SqlBackend Handler ()
createPlayerSeason userId leagueId seasonId playerId = do
    now <- liftIO getCurrentTime
    insert_ $ PlayerSeason { playerSeasonLeagueId = leagueId
                           , playerSeasonPlayerId = playerId
                           , playerSeasonSeasonId = seasonId
                           , playerSeasonTeamId = Nothing
                           , playerSeasonIsStarter = False
                           , playerSeasonTotalPoints = 0
                           , playerSeasonRegularSeasonPoints = 0
                           , playerSeasonPostSeasonPoints = 0
                           , playerSeasonCreatedBy = userId
                           , playerSeasonCreatedAt = now
                           , playerSeasonUpdatedBy = userId
                           , playerSeasonUpdatedAt = now
                           }


----------------------
-- General Settings --
----------------------
createGeneralSettings :: UserId -> SeasonId -> Entity League -> Maybe SeasonId -> ReaderT SqlBackend Handler ()
createGeneralSettings userId seasonId (Entity leagueId league) maybePreviousSeasonId = do
    now <- liftIO getCurrentTime
    let teamsCount = leagueTeamsCount league
        generalSettings = GeneralSettings
            { generalSettingsLeagueId = leagueId
            , generalSettingsSeasonId = seasonId
            , generalSettingsNumberOfStarters = fst $ defaultRosterSize teamsCount
            , generalSettingsRosterSize = snd $ defaultRosterSize teamsCount
            , generalSettingsRegularSeasonLength = fst $ defaultSeasonLength teamsCount
            , generalSettingsPlayoffLength = snd $ defaultSeasonLength teamsCount
            , generalSettingsNumberOfTeamsInPlayoffs = defaultNumberOfTeamsInPlayoffs teamsCount
            , generalSettingsTradeDeadlineWeek = defaultTradeDeadlineWeek teamsCount
            , generalSettingsWaiverPeriodInDays = defaultWaiverPeriodInDays
            , generalSettingsCreatedBy = userId
            , generalSettingsCreatedAt = now
            , generalSettingsUpdatedBy = userId
            , generalSettingsUpdatedAt = now
            }

    case maybePreviousSeasonId of
        Nothing -> insert_ generalSettings
        Just (previousSeasonId) -> do
            Entity _ prevGS <- getBy404 $ UniqueGeneralSettingsSeasonId previousSeasonId
            insert_ $ generalSettings
                { generalSettingsNumberOfStarters = generalSettingsNumberOfStarters prevGS
                , generalSettingsRosterSize = generalSettingsRosterSize prevGS
                , generalSettingsRegularSeasonLength = generalSettingsRegularSeasonLength prevGS
                , generalSettingsPlayoffLength = generalSettingsPlayoffLength prevGS
                , generalSettingsNumberOfTeamsInPlayoffs = generalSettingsNumberOfTeamsInPlayoffs prevGS
                , generalSettingsTradeDeadlineWeek = generalSettingsTradeDeadlineWeek prevGS
                , generalSettingsWaiverPeriodInDays = generalSettingsWaiverPeriodInDays prevGS
                }


----------------------
-- Scoring Settings --
----------------------
createScoringSettingsRow :: UserId -> SeasonId -> Entity League -> Maybe SeasonId -> Action ->
                            ReaderT SqlBackend Handler ()
createScoringSettingsRow userId seasonId (Entity leagueId league) maybePreviousSeasonId action = do
    now <- liftIO getCurrentTime
    let (isUsed, points, weight, pointsRec, weightRec) =
            defaultScoringAttributes action $ leagueScoringType league
        scoringSettings = ScoringSettings
            { scoringSettingsLeagueId = leagueId
            , scoringSettingsSeasonId = seasonId
            , scoringSettingsAction = action
            , scoringSettingsIsUsed = isUsed
            , scoringSettingsPoints = points
            , scoringSettingsWeight = weight
            , scoringSettingsPointsReceiving = pointsRec
            , scoringSettingsWeightReceiving = weightRec
            , scoringSettingsCreatedBy = userId
            , scoringSettingsCreatedAt = now
            , scoringSettingsUpdatedBy = userId
            , scoringSettingsUpdatedAt = now
            }

    case maybePreviousSeasonId of
        Nothing -> insert_ scoringSettings
        Just previousSeasonId -> do
            mPrevSS <- getBy $ UniqueScoringSettingsSeasonIdAction previousSeasonId action
            case mPrevSS of
                Nothing -> insert_ scoringSettings
                Just (Entity _ prevSS) ->
                    insert_ $ scoringSettings
                        { scoringSettingsIsUsed = scoringSettingsIsUsed prevSS
                        , scoringSettingsPoints = scoringSettingsPoints prevSS
                        , scoringSettingsWeight = scoringSettingsWeight prevSS
                        , scoringSettingsPointsReceiving = scoringSettingsPointsReceiving prevSS
                        , scoringSettingsWeightReceiving = scoringSettingsWeightReceiving prevSS
                        }

----------
-- Week --
----------
createWeekData :: Entity Episode -> Entity Season -> Handler (Entity Week)
createWeekData (Entity episodeId episode) (Entity seasonId season) = do
    let leagueId = seasonLeagueId season
    -- create week if week not already created for episode
    alreadyCreatedWeek <- runDB $ getBy $ UniqueWeekLeagueIdEpisodeId leagueId episodeId
    case alreadyCreatedWeek of
        Just weekEntity -> return weekEntity
        Nothing -> do
            Entity weekId week <- createWeek (Entity seasonId season) $ Entity episodeId episode
            -- then, for the week:
            teamIds <- runDB $ selectKeysList [TeamLeagueId ==. leagueId] []
            -- create game for each team in league
            mapM_ (createGame leagueId weekId) teamIds
            -- create performance for each player in league
            createPerformances episode $ Entity weekId week
            -- determine if trade deadline has passed or season is complete
            determineIfTradeDeadlineHasPassed week $ Entity seasonId season
            determineIfSeasonIsComplete episode seasonId

            return $ Entity weekId week

createWeekData_ :: Entity Episode -> Entity Season -> Handler ()
createWeekData_ episodeEntity seasonEntity = do
    _ <- createWeekData episodeEntity seasonEntity
    return ()

createWeek :: Entity Season -> Entity Episode -> Handler (Entity Week)
createWeek (Entity seasonId season) (Entity episodeId episode) = runDB $ do
    now <- liftIO getCurrentTime
    let week = Week { weekLeagueId     = seasonLeagueId season
                    , weekEpisodeId    = episodeId
                    , weekSeasonId     = seasonId
                    , weekNumber       = episodeNumber episode
                    , weekIsScored     = False
                    , weekIsPostSeason = seasonIsInPostSeason season
                    , weekCreatedAt    = now
                    , weekUpdatedAt    = now
                    }
    weekId <- insert week
    return $ Entity weekId week

backfillWeekData :: UserId -> Entity Season -> Entity Episode -> Handler ()
backfillWeekData userId seasonEntity (Entity episodeId episode) = do
    weekEntity <- createWeekData (Entity episodeId episode) seasonEntity
    events <- runDB $ selectList [EventEpisodeId ==. episodeId] [Asc EventTimeInEpisode]
    let leagueId = seasonLeagueId $ entityVal seasonEntity
    forM_ events $ createPlay leagueId weekEntity
    finalizeWeek episodeId userId leagueId

determineIfTradeDeadlineHasPassed :: Week -> Entity Season -> Handler ()
determineIfTradeDeadlineHasPassed week (Entity seasonId season) =
    if seasonIsAfterTradeDeadline season then return () else do
        isBeforeTradeDeadline <- weekNumberBeforeTradeDeadline seasonId week
        if isBeforeTradeDeadline then return () else do
            maybeAdmin <- runDB $ selectFirst [UserIsAdmin ==. True] [Asc UserId]
            let adminUserId = entityKey $ fromJust maybeAdmin
            now <- liftIO getCurrentTime
            runDB $ update seasonId [ SeasonIsAfterTradeDeadline =. True
                                    , SeasonUpdatedBy =. adminUserId
                                    , SeasonUpdatedAt =. now
                                    ]
            cancelAllTrades adminUserId seasonId

determineIfSeasonIsComplete :: Episode -> SeasonId -> Handler ()
determineIfSeasonIsComplete episode seasonId = do
    series <- runDB $ get404 $ episodeSeriesId episode
    if episodeNumber episode /= seriesTotalEpisodes series then return () else do
        maybeAdmin <- runDB $ selectFirst [UserIsAdmin ==. True] [Asc UserId]
        let adminUserId = entityKey $ fromJust maybeAdmin
        now <- liftIO getCurrentTime
        runDB $ update seasonId [ SeasonIsSeasonComplete =. True
                                , SeasonUpdatedBy =. adminUserId
                                , SeasonUpdatedAt =. now
                                ]
        cancelAllTransactionRequests adminUserId seasonId

weekNumberBeforeTradeDeadline :: SeasonId -> Week -> Handler Bool
weekNumberBeforeTradeDeadline seasonId week = runDB $ do
    Entity _ generalSettings <- getBy404 $ UniqueGeneralSettingsSeasonId seasonId
    return $ weekNumber week < generalSettingsTradeDeadlineWeek generalSettings


----------
-- Game --
----------
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


-----------------
-- Performance --
-----------------
createPerformances :: Episode -> Entity Week -> Handler ()
createPerformances episode (Entity weekId week) = do
    series <- runDB $ get404 $ episodeSeriesId episode
    let (leagueId, totalEpisodes) = (weekLeagueId week, seriesTotalEpisodes series)
        overallNumber = episodeOverallNumber episode
    previousWeeks <- getPreviousWeeks leagueId overallNumber totalEpisodes
    playerSeasons <- runDB $ selectList [PlayerSeasonSeasonId ==. weekSeasonId week] []
    mapM_ (createPerformance leagueId weekId $ map entityKey previousWeeks) playerSeasons

createPerformance :: LeagueId -> WeekId -> [WeekId] -> Entity PlayerSeason -> Handler ()
createPerformance leagueId weekId previousWeekIds (Entity _ playerSeason) = do
    now <- liftIO getCurrentTime
    let playerId = playerSeasonPlayerId playerSeason
    maybePerformance <- runDB $ getBy $ UniquePerformanceWeekIdPlayerId weekId playerId
    (cumulativePoints, cappedCumulativePoints) <- calculateCumulativePoints previousWeekIds playerId
    case maybePerformance of
        Just _ -> return ()
        Nothing -> runDB $ insert_ $ Performance
            { performanceLeagueId  = leagueId
            , performanceWeekId    = weekId
            , performancePlayerId  = playerId
            , performanceTeamId    = playerSeasonTeamId playerSeason
            , performanceIsStarter = playerSeasonIsStarter playerSeason
            , performancePoints    = 0
            , performanceCumulativePoints = cumulativePoints
            , performanceCappedCumulativePoints = cappedCumulativePoints
            , performanceCreatedAt = now
            , performanceUpdatedAt = now
            }
