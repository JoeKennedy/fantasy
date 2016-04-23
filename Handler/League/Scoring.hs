module Handler.League.Scoring where

import Import

import Handler.Common        (extractKeyMaybe, extractValue, extractValueMaybe)
import Handler.League.Setup
import Handler.League.Layout

import Data.List ((!!))

----------
-- Form --
----------
scoringSettingsForm :: UserId -> League -> [ScoringSettings] -> Form [ScoringSettings]
scoringSettingsForm currentUserId league scoringSettingsList extra = do
    let scoringType = leagueScoringType league
    forms <- do
        isUsed <- for scoringSettingsList (\scoringSettings ->
            mreq checkBoxField "" (Just $ scoringSettingsIsUsed scoringSettings))
        pointsAndWeights <- forM scoringSettingsList (\scoringSettings -> sequence
            [ mreq intField inputRight (Just $ scoringSettingsPoints scoringSettings)
            , mreq intField inputRight (Just $ scoringSettingsWeight scoringSettings)
            , mreq intField inputRight (Just $ scoringSettingsPointsReceiving scoringSettings)
            , mreq intField inputRight (Just $ scoringSettingsWeightReceiving scoringSettings)
            ])
        return $ zip3 scoringSettingsList isUsed pointsAndWeights

    let (multiCharacterSettings, singleCharacterSettings) =
            partition (\(ss, _, _) -> isMultiCharacter $ scoringSettingsAction ss) forms
    now <- liftIO getCurrentTime

    let scoringSettingsResults =
            for forms (\(scoringSettings, isUsed, pointsAndWeights) ->
                ScoringSettings
                    <$> pure (scoringSettingsLeagueId scoringSettings)
                    <*> pure (scoringSettingsAction scoringSettings)
                    <*> fst isUsed                    -- isUsedRes
                    <*> fst ((pointsAndWeights) !! 0) -- pointsRes
                    <*> weightValue    scoringSettings scoringType (fst (pointsAndWeights !! 1))
                    <*> pointsRecValue scoringSettings scoringType (fst (pointsAndWeights !! 2))
                    <*> weightRecValue scoringSettings scoringType (fst (pointsAndWeights !! 3))
                    <*> pure (scoringSettingsCreatedBy scoringSettings)
                    <*> pure (scoringSettingsCreatedAt scoringSettings)
                    <*> updatedByField currentUserId
                    <*> pure now)

    let widget = case leagueScoringType league of Weighted -> $(widgetFile "league/weighted_scoring_settings_form")
                                                  Vanilla  -> $(widgetFile "league/vanilla_scoring_settings_form")
                                                  _        -> error "This scoring type does not use this form"
    return (scoringSettingsResults, widget)


------------
-- Routes --
------------
getSetupScoringSettingsR :: Handler Html
getSetupScoringSettingsR = do
    userId <- requireAuthId
    let action = SetupLeagueR SetupScoringSettingsR
    (Entity leagueId league, lastCompletedStep) <- leagueOrRedirect userId action
    scoringSettingsList <- runDB $ selectList [ScoringSettingsLeagueId ==. leagueId] [Asc ScoringSettingsId]
    (widget, enctype) <- generateFormPost $ scoringSettingsForm userId league $ map extractValue scoringSettingsList
    defaultLayout $ do
        setTitle $ leagueSetupStepTitle league action
        let maybeLeagueId = Just leagueId
        $(widgetFile "layouts/league-setup-layout")

postSetupScoringSettingsR :: Handler Html
postSetupScoringSettingsR = do
    userId <- requireAuthId
    let action = SetupLeagueR SetupScoringSettingsR
    (Entity leagueId league, lastCompletedStep) <- leagueOrRedirect userId action
    scoringSettingsList <- runDB $ selectList [ScoringSettingsLeagueId ==. leagueId] [Asc ScoringSettingsId]
    ((result, widget), enctype) <- runFormPost $ scoringSettingsForm userId league $ map extractValue scoringSettingsList
    case result of
        FormSuccess scoringSettingsList' -> do
            forM_ (zip scoringSettingsList scoringSettingsList') (\(Entity scoringSettingsId _, scoringSettings') ->
                runDB $ replace scoringSettingsId scoringSettings')
            updateLeagueLastCompletedStep leagueId league 3
            redirect $ SetupLeagueR SetupDraftSettingsR
        _ -> defaultLayout $ do
            setTitle $ leagueSetupStepTitle league action
            let maybeLeagueId = Just leagueId
            $(widgetFile "layouts/league-setup-layout")

getLeagueScoringSettingsR :: LeagueId -> Handler Html
getLeagueScoringSettingsR leagueId = do
    userId <- requireAuthId
    league <- runDB $ get404 leagueId
    scoringSettingsList <- runDB $ selectList [ScoringSettingsLeagueId ==. leagueId] [Asc ScoringSettingsId]
    (widget, enctype) <- generateFormPost $ scoringSettingsForm userId league $ map extractValue scoringSettingsList
    let action = LeagueSettingsR leagueId LeagueScoringSettingsR
    leagueSettingsLayout leagueId action enctype widget "Scoring"

postLeagueScoringSettingsR :: LeagueId -> Handler Html
postLeagueScoringSettingsR leagueId = do
    userId <- requireAuthId
    league <- runDB $ get404 leagueId
    scoringSettingsList <- runDB $ selectList [ScoringSettingsLeagueId ==. leagueId] [Asc ScoringSettingsId]
    ((result, widget), enctype) <- runFormPost $ scoringSettingsForm userId league $ map extractValue scoringSettingsList
    let action = LeagueSettingsR leagueId LeagueScoringSettingsR
    case result of
        FormSuccess scoringSettingsList' -> do
            forM_ (zip scoringSettingsList scoringSettingsList') (\(Entity scoringSettingsId _, scoringSettings') ->
                runDB $ replace scoringSettingsId scoringSettings')
            setMessage "Successfully updated league scoring settings"
            redirect action
        _ -> leagueSettingsLayout leagueId action enctype widget "Scoring"


-------------
-- Helpers --
-------------
scoringSettingsNotUsedError :: a
scoringSettingsNotUsedError = error "This scoring type doesn't use the scoring settings table"

weightValue :: Applicative f => ScoringSettings -> ScoringType -> f Int -> f Int
weightValue scoringSettings scoringType field =
    case scoringType of Vanilla     -> pure $ scoringSettingsWeight scoringSettings
                        Weighted    -> field
                        Scorekeeper -> scoringSettingsNotUsedError

pointsRecValue :: Applicative f => ScoringSettings -> ScoringType -> f Int -> f Int
pointsRecValue scoringSettings scoringType field =
    case (scoringType, isMultiCharacter $ scoringSettingsAction scoringSettings)
        of (Scorekeeper, _)     -> scoringSettingsNotUsedError
           (_,           True)  -> field
           (_,           False) -> pure $ scoringSettingsPointsReceiving scoringSettings

weightRecValue :: Applicative f => ScoringSettings -> ScoringType -> f Int -> f Int
weightRecValue scoringSettings scoringType field =
    case (scoringType, isMultiCharacter $ scoringSettingsAction scoringSettings)
        of (Scorekeeper, _)    -> scoringSettingsNotUsedError
           (Weighted,    True) -> field
           (_,           _)    -> pure $ scoringSettingsWeightReceiving scoringSettings


---------------------------------------------------------
-- Calculate Scores -- THIS IS WHERE THE MAGIC HAPPENS --
---------------------------------------------------------

-- Need to add a well-hidden/hard to press button to click to calculate scores
-- for an episode once it's complete
calculateScores :: EpisodeId -> Handler ()
calculateScores episodeId = do
    leagueIds <- runDB $ selectKeysList [LeagueIsActive ==. True] [Asc LeagueId]
    mapM_ (calculateLeagueScores episodeId) leagueIds
    setMessage "Calculating (or calculated) scores for all leagues for this episode"

calculateLeagueScores :: EpisodeId -> LeagueId -> Handler ()
calculateLeagueScores episodeId leagueId = do
    -- check if any plays have already been created for this week
    Entity weekId _ <- runDB $ getBy404 $ UniqueWeekLeagueIdEpisodeId leagueId episodeId
    playsCount <- runDB $ count [PlayWeekId ==. weekId]
    -- if no plays exist for the week, create all the plays
    if playsCount > 0 then return () else do
        events <- runDB $ selectList [EventEpisodeId ==. episodeId] []
        plays <- mapM (createPlay leagueId weekId) events
        -- for each play, update points on the relevant performance and game
        mapM_ calculateWeekPoints plays

        -- for each team, update total points on the season
        games <- runDB $ selectList [GameWeekId ==. weekId] []
        teams <- mapM addToTeamPoints games
        -- then compare totals to determine waiver order
        determineWaiverOrder teams
        -- for each player update total points on the season
        performances <- runDB $ selectList [PerformanceWeekId ==. weekId] []
        mapM_ addToPlayerPoints performances
        -- mark the week scored
        now <- liftIO getCurrentTime
        runDB $ update weekId [WeekIsScored =. True, WeekUpdatedAt =. now]

calculateWeekPoints :: Play -> Handler ()
calculateWeekPoints play = do
    let weekId = playWeekId play
    -- add points to performance and game for player
    addPointsToPerformanceAndGame weekId (playPlayerId play) (playTeamId play) (playPoints play)
    case playReceivingPlayerId play of
        Nothing       -> return ()
        -- add points to performance and game for receiving player if relevant
        Just receivingPlayerId ->
            addPointsToPerformanceAndGame weekId receivingPlayerId
                    (playReceivingTeamId play) (playReceivingPoints play)

addPointsToPerformanceAndGame :: WeekId -> PlayerId -> Maybe TeamId -> Rational -> Handler ()
addPointsToPerformanceAndGame weekId playerId maybeTeamId points = do
    Entity performanceId performance <- runDB $ getBy404 $ UniquePerformanceWeekIdPlayerId weekId playerId
    now <- liftIO getCurrentTime
    -- add points to the player's peformance for this week
    runDB $ update performanceId [ PerformancePoints    +=. points
                                 , PerformanceUpdatedAt  =. now
                                 ]
    case (maybeTeamId, performanceIsStarter performance) of
        -- if the player is a starter on a team, add points to this team's game for this week
        (Just teamId, True) -> runDB $ do
            Entity gameId _ <- getBy404 $ UniqueGameWeekIdTeamId weekId teamId
            update gameId [GamePoints +=. points, GameUpdatedAt =. now]
        (_, _) -> return ()

addToTeamPoints :: Entity Game -> Handler (Entity Team)
addToTeamPoints (Entity _gameId game) = do
    let teamId = gameTeamId game
    now <- liftIO getCurrentTime
    runDB $ update teamId [TeamPointsThisSeason +=. gamePoints game, TeamUpdatedAt =. now]
    team <- runDB $ get404 teamId
    return $ Entity teamId team

determineWaiverOrder :: [Entity Team] -> Handler ()
determineWaiverOrder teams = do
    -- sort teams by points this season ascending and make that the waiver order
    let sortedTeams = sortOn (teamPointsThisSeason . extractValue) teams
        withWaiverOrder = zip [1..] sortedTeams
    mapM_ updateWaiverOrder withWaiverOrder

updateWaiverOrder :: (Int, Entity Team) -> Handler ()
updateWaiverOrder (waiverOrder, Entity teamId _) = do
    now <- liftIO getCurrentTime
    runDB $ update teamId [TeamWaiverOrder =. waiverOrder, TeamUpdatedAt =. now]

addToPlayerPoints :: Entity Performance -> Handler ()
addToPlayerPoints (Entity _performanceId performance) = do
    let (playerId, points) = (performancePlayerId performance, performancePoints performance)
    now <- liftIO getCurrentTime
    runDB $ update playerId [PlayerPointsThisSeason +=. points, PlayerUpdatedAt =. now]


--------------
-- Creators --
--------------
createPlay :: LeagueId -> WeekId -> Entity Event -> Handler Play
createPlay leagueId weekId (Entity eventId event) = do
    (points, pointsRec, Entity playerId player, mRecPlayer) <- calculatePointsAndPlayers leagueId event
    now <- liftIO getCurrentTime
    let play = Play { playLeagueId = leagueId
                    , playWeekId   = weekId
                    , playEventId  = eventId
                    , playPlayerId = playerId
                    , playTeamId   = playerTeamId player
                    , playPoints   = points
                    , playAction   = eventAction event
                    , playReceivingPlayerId = extractKeyMaybe mRecPlayer
                    , playReceivingTeamId   = join $ mapM playerTeamId $ extractValueMaybe mRecPlayer
                    , playReceivingPoints   = pointsRec
                    , playNote = eventNote event
                    , playCreatedAt = now
                    , playUpdatedAt = now
                    }
    _playId <- runDB $ insert play
    return play

calculatePointsAndPlayers :: LeagueId -> Event -> Handler (Rational, Rational, Entity Player, Maybe (Entity Player))
calculatePointsAndPlayers leagueId event = do
    let characterId = eventCharacterId event
        receivingCharacterId = eventReceivingCharacterId event

    character <- runDB $ get404 characterId
    maybeReceivingCharacter <- runDB $ mapM get404 receivingCharacterId

    Entity playerId player <- runDB $ getBy404 $ UniquePlayerLeagueIdCharacterId leagueId characterId
    maybeReceivingPlayer <- runDB $ mapM (getBy404 . UniquePlayerLeagueIdCharacterId leagueId) receivingCharacterId

    Entity _ scoringSettings <- runDB $ getBy404 $ UniqueScoringSettingsLeagueIdAction leagueId $ eventAction event

    let existingPoints = playerPointsThisSeason player + (toRational $ characterPointsLastSeason character)
        lastSeasonReceiving = toRational $ fromMaybe 0 $ map characterPointsLastSeason maybeReceivingCharacter
        thisSeasonReceiving = fromMaybe 0 $ map playerPointsThisSeason $ extractValueMaybe maybeReceivingPlayer
        existingPointsReceiving = lastSeasonReceiving + thisSeasonReceiving
        (points, pointsRec) = calculateWeightedPoints scoringSettings existingPoints existingPointsReceiving

    return (points, pointsRec, (Entity playerId player), maybeReceivingPlayer)

calculateWeightedPoints :: ScoringSettings -> Rational -> Rational -> (Rational, Rational)
calculateWeightedPoints scoringSettings existingPoints existingPointsReceiving =
    if scoringSettingsIsUsed scoringSettings then
        let points    = toRational $ scoringSettingsPoints scoringSettings
            weight    = (toRational $ scoringSettingsWeight scoringSettings) / 100
            pointsRec = toRational $ scoringSettingsPointsReceiving scoringSettings
            weightRec = (toRational $ scoringSettingsWeightReceiving scoringSettings) / 100
            -- If existing points are negative, round up to zero. Don't kick any
            -- characters while they're down.
            exist     = max existingPoints 0
            existRec  = max existingPointsReceiving 0
        in  if isMultiCharacter $ scoringSettingsAction scoringSettings
                -- multi-character actions
                then if weightRec >= 0
                    -- if receiving weight is positive, weight the existing points of the opposite player
                    then (weight * existRec + points, weightRec * exist + pointsRec)
                    -- if receiving weight is negative, weight the existing points of the receiving player only
                    else (weight * existRec + points, weightRec * existRec + pointsRec)
                -- single character actions weight the existing points of that player
                else (weight * exist + points, 0)
    else (0, 0)

