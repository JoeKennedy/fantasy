module Handler.League where

import Import
import Handler.Common        (extractKeyMaybe, extractValue, extractValueMaybe)
import Handler.League.Setup
import Handler.League.Layout
import Handler.League.Week   (createWeekData)

import Data.Random.List
import Data.Random.RVar
import Data.Random.Source.DevRandom
import Network.Mail.Mime
import System.Random                (newStdGen)

----------
-- Form --
----------
leagueForm :: UserId -> Maybe League -> Form League
leagueForm currentUserId league extra = do
    (nameRes, nameView) <- mreq textField (fieldName "Name") (leagueName <$> league)
    (isPrivateRes, isPrivateView) <- mreq checkBoxField "Is league private?"
        (leagueIsPrivate <$> league)
    (scoringTypeRes, scoringTypeView) <- mreq hiddenField (hidden "Scoring type")
        (leagueScoringType <$> league)
    let teamsCount = leagueTeamsCount <$> league
    (teamsCountRes, teamsCountView) <- mreq (selectFieldList $ teamsCountOptions teamsCount)
        (fieldName "Number of teams") teamsCount

    now <- liftIO getCurrentTime
    let leagueResult = League
            <$> nameRes
            <*> existingElseDefault True (leagueIsActive <$> league)
            <*> isPrivateRes
            <*> scoringTypeRes
            <*> teamsCountRes
            <*> existingElseDefault False (leagueIsSetupComplete <$> league)
            <*> existingElseDefault 1 (leagueLastCompletedStep <$> league)
            <*> existingElseDefault False (leagueIsDraftComplete <$> league)
            <*> createdByField currentUserId (leagueCreatedBy <$> league)
            <*> existingElseDefault now (leagueCreatedAt <$> league)
            <*> updatedByField currentUserId
            <*> pure now
            <*> existingElseDefault Nothing (leagueDraftCompletedAt <$> league)
    return (leagueResult, $(widgetFile "league/league_info_form"))


------------
-- Routes --
------------
getLeaguesR :: Handler Html
getLeaguesR = do
    leagues <- runDB $ selectList [LeagueIsPrivate ==. False, LeagueIsSetupComplete ==. True] [Asc LeagueName]
    defaultLayout $ do
        setTitle "Leagues"
        $(widgetFile "league/leagues")

getLeagueR :: LeagueId -> Handler Html
getLeagueR leagueId = do
    league <- runDB $ get404 leagueId
    teams <- runDB $ selectList [TeamLeagueId ==. leagueId] [Asc TeamId]
    leagueLayout leagueId "League" $ do
        let maybeCreatorTeam = listToMaybe teams
        $(widgetFile "league/league")

postLeagueCancelR :: LeagueId -> Handler ()
postLeagueCancelR leagueId = do
    userId <- requireAuthId
    now <- liftIO getCurrentTime
    stdgen <- liftIO newStdGen
    -- Give the league a random name to avoid conflicting with new leagues
    let newLeagueName = "Canceled League " ++ toPathPiece leagueId
        randomText    = pack $ fst $ randomString 24 stdgen
    runDB $ update leagueId [ LeagueName =. newLeagueName ++ randomText
                            , LeagueIsActive =. False
                            , LeagueUpdatedBy =. userId
                            , LeagueUpdatedAt =. now
                            ]
    setMessage "Your league has been successfully canceled"

getSetupNewLeagueR :: Handler Html
getSetupNewLeagueR = do
    userId <- requireAuthId
    maybeLeague <- leagueBeingSetUp userId
    (widget, enctype) <- generateFormPost $ leagueForm userId $ extractValueMaybe maybeLeague
    defaultLayout $ do
        let title = "Create A League!" :: Html
            action = SetupLeagueR SetupNewLeagueR
            lastCompletedStep = fromMaybe 0 (leagueLastCompletedStep <$> extractValueMaybe maybeLeague)
            maybeLeagueId = extractKeyMaybe maybeLeague
        setTitle title
        $(widgetFile "layouts/league-setup-layout")

postSetupNewLeagueR :: Handler Html
postSetupNewLeagueR = do
    userId <- requireAuthId
    maybeLeague <- leagueBeingSetUp userId
    ((result, widget), enctype) <- runFormPost $ leagueForm userId $ extractValueMaybe maybeLeague
    case result of
        FormSuccess league -> do
            case maybeLeague of Just (Entity lId _) -> runDB $ replace lId league
                                Nothing             -> createLeague league
            redirect $ SetupLeagueR SetupGeneralSettingsR
        _ -> defaultLayout $ do
            let title = "Create A League!" :: Html
                action = SetupLeagueR SetupNewLeagueR
                lastCompletedStep = fromMaybe 0 (leagueLastCompletedStep <$> extractValueMaybe maybeLeague)
                maybeLeagueId = extractKeyMaybe maybeLeague
            setTitle title
            $(widgetFile "layouts/league-setup-layout")

getLeagueEditSettingsR :: LeagueId -> Handler Html
getLeagueEditSettingsR leagueId = do
    userId <- requireAuthId
    league <- runDB $ get404 leagueId
    (widget, enctype) <- generateFormPost $ leagueForm userId $ Just league
    let action = LeagueSettingsR leagueId LeagueEditSettingsR
    leagueSettingsLayout leagueId action enctype widget "League"

postLeagueEditSettingsR :: LeagueId -> Handler Html
postLeagueEditSettingsR leagueId = do
    userId <- requireAuthId
    league <- runDB $ get404 leagueId
    ((result, widget), enctype) <- runFormPost $ leagueForm userId $ Just league
    let action = LeagueSettingsR leagueId LeagueEditSettingsR
    case result of
        FormSuccess league' -> do
            runDB $ replace leagueId league'
            setMessage "Successfully updated league settings"
            redirect action
        _ -> leagueSettingsLayout leagueId action enctype widget "League"

-------------------
-- Create League --
-------------------
createLeague :: League -> Handler ()
createLeague league = do
    let teamNumbers = [1..(leagueTeamsCount league)]
    draftOrder <- liftIO (runRVar (shuffle teamNumbers) DevRandom :: IO [Int])
    leagueId <- runDB $ insert league
    runDB $ do
        let teamsCount = leagueTeamsCount league
            leagueEntity = Entity leagueId league
        insert_ $ GeneralSettings
            { generalSettingsLeagueId = leagueId
            , generalSettingsNumberOfStarters = fst $ defaultRosterSize teamsCount
            , generalSettingsRosterSize = snd $ defaultRosterSize teamsCount
            , generalSettingsRegularSeasonLength = fst $ defaultSeasonLength teamsCount
            , generalSettingsPlayoffLength = snd $ defaultSeasonLength teamsCount
            , generalSettingsNumberOfTeamsInPlayoffs = defaultNumberOfTeamsInPlayoffs teamsCount
            , generalSettingsTradeDeadlineWeek = defaultTradeDeadlineWeek teamsCount
            , generalSettingsWaiverPeriodInDays = defaultWaiverPeriodInDays
            , generalSettingsCreatedBy = leagueCreatedBy league
            , generalSettingsCreatedAt = leagueCreatedAt league
            , generalSettingsUpdatedBy = leagueUpdatedBy league
            , generalSettingsUpdatedAt = leagueUpdatedAt league
            }
        mapM_ (createScoringSettingsRow leagueEntity) allActions
        mapM_ (createTeam leagueEntity) $ zip teamNumbers draftOrder
        characters <- selectList [] [Asc CharacterName]
        mapM_ (createPlayer leagueEntity) characters

    -- create week and related data for any already aired episodes this season
    maybeSeries <- runDB $ selectFirst [] [Asc SeriesNumber]
    case maybeSeries of
        Nothing -> return ()
        Just (Entity seriesId _) -> do
            episodes <- runDB $ selectList [ EpisodeSeriesId ==. seriesId
                                           , EpisodeStatus !=. YetToAir
                                           ] [Asc EpisodeId]
            mapM_ (\e -> createWeekData e leagueId) episodes
            mapM_ (\(Entity eid _) -> calculateLeagueScores eid leagueId) episodes

createScoringSettingsRow :: Entity League -> Action -> ReaderT SqlBackend Handler ()
createScoringSettingsRow (Entity leagueId league) action =
    let (isUsed, points, weight, pointsRec, weightRec) =
            defaultScoringAttributes action $ leagueScoringType league
    in  insert_ $ ScoringSettings
            { scoringSettingsLeagueId = leagueId
            , scoringSettingsAction = action
            , scoringSettingsIsUsed = isUsed
            , scoringSettingsPoints = points
            , scoringSettingsWeight = weight
            , scoringSettingsPointsReceiving = pointsRec
            , scoringSettingsWeightReceiving = weightRec
            , scoringSettingsCreatedBy = leagueCreatedBy league
            , scoringSettingsCreatedAt = leagueCreatedAt league
            , scoringSettingsUpdatedBy = leagueUpdatedBy league
            , scoringSettingsUpdatedAt = leagueUpdatedAt league
            }

createTeam :: Entity League -> (Int, Int) -> ReaderT SqlBackend Handler ()
createTeam (Entity leagueId league) (teamNumber, draftOrder) = do
    stdgen <- liftIO newStdGen
    let (name, abbrev, owner, email) = teamTextAttributes teamNumber
        (maybeTeamOwnerId, maybeConfirmedAt) = teamNonTextAttributes league teamNumber
        verificationKey = pack $ fst $ randomString 24 stdgen
    insert_ $ Team { teamLeagueId         = leagueId
                   , teamName             = name
                   , teamAbbreviation     = abbrev
                   , teamOwnerId          = maybeTeamOwnerId
                   , teamOwnerName        = owner
                   , teamOwnerEmail       = email
                   , teamIsConfirmed      = isJust maybeConfirmedAt
                   , teamPlayersCount     = 0
                   , teamStartersCount    = 0
                   , teamDraftOrder       = draftOrder
                   , teamWaiverOrder      = teamNumber
                   , teamVerificationKey  = verificationKey
                   , teamPointsThisSeason = 0
                   , teamCreatedBy        = leagueCreatedBy league
                   , teamCreatedAt        = leagueCreatedAt league
                   , teamUpdatedBy        = leagueUpdatedBy league
                   , teamUpdatedAt        = leagueUpdatedAt league
                   , teamConfirmedBy      = maybeTeamOwnerId
                   , teamConfirmedAt      = maybeConfirmedAt
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

createPlayer :: Entity League -> Entity Character -> ReaderT SqlBackend Handler ()
createPlayer (Entity leagueId league) (Entity characterId character) =
    insert_ $ Player { playerLeagueId         = leagueId
                     , playerCharacterId      = characterId
                     , playerTeamId           = Nothing
                     , playerIsStarter        = False
                     , playerPointsThisSeason = 0
                     , playerIsPlayable       = characterIsPlayable character
                     , playerCreatedBy        = leagueCreatedBy league
                     , playerCreatedAt        = leagueCreatedAt league
                     , playerUpdatedBy        = leagueUpdatedBy league
                     , playerUpdatedAt        = leagueUpdatedAt league
                     }

-------------
-- Helpers --
-------------
scoringTypeWidget :: ScoringType -> Widget
scoringTypeWidget scoringType = $(widgetFile "league/scoring_type")

leagueListGroupItem :: Maybe League -> ScoringType -> Widget
leagueListGroupItem (Just league) scoringType
    | leagueScoringType league == scoringType =
        [whamlet|<div .list-group-item .active>^{scoringTypeWidget scoringType}|]
    | otherwise = [whamlet|<div .list-group-item>^{scoringTypeWidget scoringType}|]
leagueListGroupItem Nothing scoringType
    | isDisabledScoringType scoringType =
        [whamlet|<div .list-group-item .disabled>^{scoringTypeWidget scoringType}|]
    | otherwise = [whamlet|<a .list-group-item href="#">^{scoringTypeWidget scoringType}|]


---------------------------------------------------------
-- Calculate Scores -- THIS IS WHERE THE MAGIC HAPPENS --
---------------------------------------------------------

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
    player <- runDB $ get404 playerId
    let pointsToAdd = if playerIsPlayable player then points else 0
    Entity performanceId performance <- runDB $ getBy404 $ UniquePerformanceWeekIdPlayerId weekId playerId
    now <- liftIO getCurrentTime
    -- add points to the player's peformance for this week
    runDB $ update performanceId [ PerformancePoints    +=. pointsToAdd
                                 , PerformanceUpdatedAt  =. now
                                 ]
    case (maybeTeamId, performanceIsStarter performance) of
        -- if the player is a starter on a team, add points to this team's game for this week
        (Just teamId, True) -> runDB $ do
            Entity gameId _ <- getBy404 $ UniqueGameWeekIdTeamId weekId teamId
            update gameId [GamePoints +=. pointsToAdd, GameUpdatedAt =. now]
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

