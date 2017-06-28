module Handler.League.Season where

import Import


-- TODO - get rid of this function
createSeries6Seasons :: Handler ()
createSeries6Seasons = do
    total <- runDB $ count [SeriesYear ==. 2016]
    if total == 1 then return () else do
        -- First update series years
        seriesList <- runDB $ selectList [] [Asc SeriesId]
        mapM_ updateSeriesYear seriesList
        -- Then create seasons
        seriesEntity <- runDB $ getBy404 $ UniqueSeriesNumber 6
        createLeagueSeasons seriesEntity

-- TODO - remove this function
updateSeriesYear :: Entity Series -> Handler ()
updateSeriesYear (Entity seriesId series) =
    runDB $ update seriesId [SeriesYear =. seriesNumber series + 2010]

-- TODO - change these methods to work with adding a new season
createLeagueSeasons :: Entity Series -> Handler ()
createLeagueSeasons seriesEntity = do
    leagues <- runDB $ selectList [] [Asc LeagueId]
    forM_ leagues $ createLeagueSeason seriesEntity

createLeagueSeason :: Entity Series -> Entity League -> Handler ()
createLeagueSeason (Entity seriesId series) (Entity leagueId league) = runDB $ do
    -- Create Season
    seasonId <- createSeasonUnlessExists (Entity seriesId series) $ Entity leagueId league

    -- Make all other seasons inactive
    inactivateOtherSeasons leagueId seasonId series

    -- Create Team Seasons
    teams <- selectList [TeamLeagueId ==. leagueId] []
    forM_ teams $ createTeamSeason leagueId seasonId series

    -- Create Player Seasons
    players <- selectList [PlayerLeagueId ==. leagueId] []
    forM_ players $ createPlayerSeason leagueId seasonId series

    -- Update Settings to use SeasonId -- TODO - make these inserts
    updateSettings leagueId seasonId series

    -- Update Weeks to use SeasonId -- TODO -- make this an insert
    now <- liftIO getCurrentTime
    updateWhere [WeekLeagueId ==. leagueId]
                [WeekSeasonId =. Just seasonId, WeekUpdatedAt =. now]

    -- Update Transactions to use Season -- TODO Remove this
    updateWhere [ TransactionLeagueId ==. leagueId ]
                [ TransactionSeasonId =. Just seasonId
                , TransactionUpdatedAt =. now
                , TransactionUpdatedBy =. seriesCreatedBy series
                ]

inactivateOtherSeasons :: LeagueId -> SeasonId -> Series -> ReaderT SqlBackend Handler ()
inactivateOtherSeasons leagueId seasonId series = do
    now <- liftIO getCurrentTime
    updateWhere [ SeasonId !=. seasonId, SeasonLeagueId ==. leagueId ]
                [ SeasonIsActive =. False
                , SeasonUpdatedBy =. seriesCreatedBy series
                , SeasonUpdatedAt =. now
                ]

createSeason :: Entity Series -> Entity League -> ReaderT SqlBackend Handler SeasonId
createSeason (Entity seriesId series) (Entity leagueId league) = do
    -- TODO - don't use the league info; initialize all to False
    now <- liftIO getCurrentTime
    insert $ Season { seasonLeagueId = leagueId
                    , seasonYear = seriesYear series
                    , seasonSeriesId = seriesId
                    , seasonIsActive = True
                    , seasonIsDraftComplete = leagueIsDraftComplete league
                    , seasonIsInPostSeason  = leagueIsInPostSeason league
                    , seasonIsAfterTradeDeadline = leagueIsAfterTradeDeadline league
                    , seasonIsSeasonComplete = leagueIsSeasonComplete league
                    , seasonCreatedBy = seriesCreatedBy series
                    , seasonCreatedAt = now
                    , seasonUpdatedBy = seriesCreatedBy series
                    , seasonUpdatedAt = now
                    , seasonDraftCompletedAt = leagueDraftCompletedAt league
                    }

createSeasonUnlessExists :: Entity Series -> Entity League -> ReaderT SqlBackend Handler SeasonId
createSeasonUnlessExists (Entity seriesId series) (Entity leagueId league) = do
    maybeSeason <- getBy $ UniqueSeasonLeagueIdSeriesId leagueId seriesId
    case map entityKey maybeSeason of
        Just seasonId -> return seasonId
        Nothing -> createSeason (Entity seriesId series) $ Entity leagueId league

createTeamSeason :: LeagueId -> SeasonId -> Series -> Entity Team -> ReaderT SqlBackend Handler ()
createTeamSeason leagueId seasonId series (Entity teamId team) = do
    -- TODO - don't use the team info; initialize all to False/0
    now <- liftIO getCurrentTime
    insert_ $ TeamSeason { teamSeasonLeagueId = leagueId
                         , teamSeasonTeamId = teamId
                         , teamSeasonSeasonId = seasonId
                         , teamSeasonPlayersCount = teamPlayersCount team
                         , teamSeasonStartersCount = teamStartersCount team
                         , teamSeasonDraftOrder = teamDraftOrder team
                         , teamSeasonWaiverOrder = teamWaiverOrder team
                         , teamSeasonTotalPoints = teamPointsThisSeason team
                         , teamSeasonRegularSeasonPoints = teamPointsThisRegularSeason team
                         , teamSeasonPostSeasonPoints = teamPointsThisPostSeason team
                         , teamSeasonPostSeasonStatus = teamPostSeasonStatus team
                         , teamSeasonCreatedBy = seriesCreatedBy series
                         , teamSeasonCreatedAt = now
                         , teamSeasonUpdatedBy = seriesCreatedBy series
                         , teamSeasonUpdatedAt = now
                         }

createPlayerSeason :: LeagueId -> SeasonId -> Series -> Entity Player -> ReaderT SqlBackend Handler ()
createPlayerSeason leagueId seasonId series (Entity playerId player) = do
    -- TODO - don't use the player info, initialize all to False/0
    now <- liftIO getCurrentTime
    insert_ $ PlayerSeason { playerSeasonLeagueId = leagueId
                           , playerSeasonPlayerId = playerId
                           , playerSeasonSeasonId = seasonId
                           , playerSeasonTeamId = playerTeamId player
                           , playerSeasonIsStarter = playerIsStarter player
                           , playerSeasonTotalPoints = playerPointsThisSeason player
                           , playerSeasonRegularSeasonPoints = playerPointsThisRegularSeason player
                           , playerSeasonPostSeasonPoints = playerPointsThisPostSeason player
                           , playerSeasonCreatedBy = seriesCreatedBy series
                           , playerSeasonCreatedAt = now
                           , playerSeasonUpdatedBy = seriesCreatedBy series
                           , playerSeasonUpdatedAt = now
                           }

updateSettings :: LeagueId -> SeasonId -> Series -> ReaderT SqlBackend Handler ()
updateSettings leagueId seasonId series = do
    -- TODO - rename to createSettings or insertSettings and change all these to inserts
    now <- liftIO getCurrentTime
    updateWhere [ GeneralSettingsLeagueId ==. leagueId ]
                [ GeneralSettingsSeasonId  =. Just seasonId
                , GeneralSettingsUpdatedAt =. now
                , GeneralSettingsUpdatedBy =. seriesCreatedBy series
                ]
    updateWhere [ ScoringSettingsLeagueId ==. leagueId ]
                [ ScoringSettingsSeasonId  =. Just seasonId
                , ScoringSettingsUpdatedAt =. now
                , ScoringSettingsUpdatedBy =. seriesCreatedBy series
                ]
    updateWhere [ DraftSettingsLeagueId ==. leagueId ]
                [ DraftSettingsSeasonId  =. Just seasonId
                , DraftSettingsUpdatedAt =. now
                , DraftSettingsUpdatedBy =. seriesCreatedBy series
                ]


