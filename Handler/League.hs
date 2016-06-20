module Handler.League where

import Import
import Handler.Common        (extractKeyMaybe, extractValueMaybe)
import Handler.League.Setup
import Handler.League.Layout
import Handler.League.Week   (createWeekData_, createWeekData, updateCumulativePoints)

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
            <*> existingElseDefault False (leagueIsInPostSeason <$> league)
            <*> existingElseDefault False (leagueIsAfterTradeDeadline <$> league)
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
    teams <- runDB $ selectList [TeamLeagueId ==. leagueId] [Desc TeamPointsThisRegularSeason]
    let maybeCreatorTeam = find (\(Entity _ t) -> teamOwnerId t == Just (leagueCreatedBy league)) teams
    leagueLayout leagueId "League" $(widgetFile "league/league")

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
    maybeSeries <- runDB $ selectFirst [] [Desc SeriesNumber]
    case maybeSeries of
        Nothing -> return ()
        Just (Entity seriesId _) -> do
            episodes <- runDB $ selectList [ EpisodeSeriesId ==. seriesId
                                           , EpisodeStatus !=. YetToAir
                                           ] [Asc EpisodeId]
            mapM_ (backfillWeekData leagueId) episodes

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
                   , teamPointsThisRegularSeason = 0
                   , teamPointsThisPostSeason = 0
                   , teamPostSeasonStatus = Regular
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
                     , playerPointsThisRegularSeason = 0
                     , playerPointsThisPostSeason = 0
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


--------------------------------
-- Move Leagues To Postseason --
--------------------------------
moveLeaguesToPostSeason :: EpisodeId -> Handler ()
moveLeaguesToPostSeason episodeId = do
    leagueIds <- runDB $ selectKeysList [ LeagueIsActive ==. True
                                        , LeagueIsInPostSeason ==. False
                                        ] [Asc LeagueId]
    mapM_ (moveLeagueToPostSeason episodeId) leagueIds

moveLeagueToPostSeason :: EpisodeId -> LeagueId -> Handler ()
moveLeagueToPostSeason episodeId leagueId = runDB $ do
    Entity _ week <- getBy404 $ UniqueWeekLeagueIdEpisodeId leagueId episodeId
    Entity _ generalSettings <- getBy404 $ UniqueGeneralSettingsLeagueId leagueId
    let lastRegularSeasonWeek = generalSettingsRegularSeasonLength generalSettings

    -- move league to postseason if this week is the last week of regular season
    if weekNumber week < lastRegularSeasonWeek then return () else do
        -- first, split teams into playoffs and consolation
        teamIds <- selectKeysList [TeamLeagueId ==. leagueId]
                                  [Desc TeamPointsThisRegularSeason]
        let teamsInPlayoffs = generalSettingsNumberOfTeamsInPlayoffs generalSettings
            (playoffTeamIds, consolationTeamIds) = splitAt teamsInPlayoffs teamIds
        mapM_ (moveTeamToPostSeason Playoff) playoffTeamIds
        mapM_ (moveTeamToPostSeason Consolation) consolationTeamIds

        -- then, set league to being in postseason
        now <- liftIO getCurrentTime
        update leagueId [LeagueIsInPostSeason =. True, LeagueUpdatedAt =. now]

moveTeamToPostSeason :: PostSeasonStatus -> TeamId -> ReaderT SqlBackend Handler ()
moveTeamToPostSeason postSeasonStatus teamId = do
    now <- liftIO getCurrentTime
    update teamId [TeamPostSeasonStatus =. postSeasonStatus, TeamUpdatedAt =. now]


---------------------------------------------------------
-- Calculate Scores -- THIS IS WHERE THE MAGIC HAPPENS --
---------------------------------------------------------

calculateScores :: EpisodeId -> Handler ()
calculateScores episodeId = do
    leagues <- runDB $ selectList [LeagueIsActive ==. True] [Asc LeagueId]
    mapM_ (calculateLeagueScores episodeId) leagues
    setMessage "Calculating (or calculated) scores for all leagues for this episode"

calculateLeagueScores :: EpisodeId -> Entity League -> Handler ()
calculateLeagueScores episodeId (Entity leagueId league) = do
    -- check if any plays have already been created for this week
    episode <- runDB $ get404 episodeId
    weekId <- createWeekData (Entity episodeId episode) leagueId
    week <- runDB $ get404 weekId
    playsCount <- runDB $ count [PlayWeekId ==. weekId]
    -- if no plays exist for the week, create all the plays
    if playsCount > 0 || weekIsScored week then return () else do
        events <- runDB $ selectList [EventEpisodeId ==. episodeId] []
        plays <- mapM (createPlay leagueId weekId) events
        -- for each play, update points on the relevant performance and game
        mapM_ calculateWeekPoints plays

        -- for each team, update total points on the season
        games <- runDB $ selectList [GameWeekId ==. weekId] []
        mapM_ (addToTeamPoints league) games
        -- then compare totals to determine waiver order
        determineWaiverOrder $ Entity leagueId league
        -- for each player update total points on the season
        performances <- runDB $ selectList [PerformanceWeekId ==. weekId] []
        mapM_ addToPlayerPoints performances
        -- if the next week exists, update cumulative points for that week
        let nextEpisodeNumber = episodeNumber episode + 1
        nextWeek <- runDB $ getBy $ UniqueWeekLeagueIdWeekNumber leagueId nextEpisodeNumber
        case nextWeek of
            Nothing -> return ()
            Just (Entity nextWeekId _) -> do
                nextWeekPerformances <- runDB $ selectList [PerformanceWeekId ==. nextWeekId] []
                mapM_ updateCumulativePoints nextWeekPerformances
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

addToTeamPoints :: League -> Entity Game -> Handler ()
addToTeamPoints league (Entity _gameId game) = runDB $ do
    let teamId = gameTeamId game
    now <- liftIO getCurrentTime
    update teamId [ TeamPointsThisSeason +=. gamePoints game
                  , if leagueIsInPostSeason league
                        then TeamPointsThisPostSeason +=. gamePoints game
                        else TeamPointsThisRegularSeason +=. gamePoints game
                  , TeamUpdatedAt =. now
                  ]

determineWaiverOrder :: Entity League -> Handler ()
determineWaiverOrder (Entity leagueId league) = do
    -- order teams by points (in regular or postseason) and make that the waiver order
    teams <- runDB $ selectList [TeamLeagueId ==. leagueId]
                                [if leagueIsInPostSeason league
                                     then Asc TeamPointsThisRegularSeason
                                     else Asc TeamPointsThisPostSeason
                                        , Asc TeamPointsThisRegularSeason
                                ]
    mapM_ updateWaiverOrder $ rank teams

updateWaiverOrder :: (Int, Entity Team) -> Handler ()
updateWaiverOrder (waiverOrder, Entity teamId _) = do
    now <- liftIO getCurrentTime
    runDB $ update teamId [TeamWaiverOrder =. waiverOrder, TeamUpdatedAt =. now]

addToPlayerPoints :: Entity Performance -> Handler ()
addToPlayerPoints (Entity _performanceId performance) = do
    let (playerId, points) = (performancePlayerId performance, performancePoints performance)
    league <- runDB $ get404 $ performanceLeagueId performance
    now <- liftIO getCurrentTime
    runDB $ update playerId [ PlayerPointsThisSeason +=. points
                            , if leagueIsInPostSeason league
                                  then PlayerPointsThisPostSeason +=. points
                                  else PlayerPointsThisRegularSeason +=. points
                            , PlayerUpdatedAt =. now
                            ]


--------------
-- Creators --
--------------
backfillWeekData :: LeagueId -> Entity Episode -> Handler ()
backfillWeekData leagueId (Entity episodeId episode) = do
    createWeekData_ (Entity episodeId episode) leagueId
    league <- runDB $ get404 leagueId
    calculateLeagueScores episodeId $ Entity leagueId league

createPlay :: LeagueId -> WeekId -> Entity Event -> Handler Play
createPlay leagueId weekId (Entity eventId event) = do
    (aPoints, wPoints, aRecPnts, wRecPnts, Entity playerId player, mRecPlayer) <- calculatePointsAndPlayers leagueId weekId event

    now <- liftIO getCurrentTime
    let play = Play { playLeagueId     = leagueId
                    , playWeekId       = weekId
                    , playEventId      = eventId
                    , playPlayerId     = playerId
                    , playTeamId       = playerTeamId player
                    , playPoints       = aPoints + wPoints
                    , playActionPoints = aPoints
                    , playWeightPoints = wPoints
                    , playAction       = eventAction event
                    , playReceivingPlayerId     = extractKeyMaybe mRecPlayer
                    , playReceivingTeamId       = join $ mapM playerTeamId $ extractValueMaybe mRecPlayer
                    , playReceivingPoints       = aRecPnts + wRecPnts
                    , playReceivingActionPoints = aRecPnts
                    , playReceivingWeightPoints = wRecPnts
                    , playNote = eventNote event
                    , playCreatedAt = now
                    , playUpdatedAt = now
                    }
    _playId <- runDB $ insert play
    return play

calculatePointsAndPlayers :: LeagueId -> WeekId -> Event ->
                             Handler (Rational, Rational, Rational, Rational, Entity Player, Maybe (Entity Player))
calculatePointsAndPlayers leagueId weekId event = runDB $ do
    let (charId, recCharId) = (eventCharacterId event, eventReceivingCharacterId event)
    Entity playerId player <- getBy404 $ UniquePlayerLeagueIdCharacterId leagueId charId
    maybeReceivingPlayer <- mapM (getBy404 . UniquePlayerLeagueIdCharacterId leagueId) recCharId

    let recPlayerId = extractKeyMaybe maybeReceivingPlayer
    Entity _ performance <- getBy404 $ UniquePerformanceWeekIdPlayerId weekId playerId
    maybeReceivingPerformance <- mapM (getBy404 . UniquePerformanceWeekIdPlayerId weekId) recPlayerId

    Entity _ scoringSettings <- getBy404 $ UniqueScoringSettingsLeagueIdAction leagueId $ eventAction event

    let cumulative = performanceCappedCumulativePoints performance
        cumulativeRec = fromMaybe 0 $ map performanceCappedCumulativePoints $ extractValueMaybe maybeReceivingPerformance
        (actionPoints, weightPoints, aPointsRec, wPointsRec) = calculatePoints scoringSettings cumulative cumulativeRec

    return (actionPoints, weightPoints, aPointsRec, wPointsRec, (Entity playerId player), maybeReceivingPlayer)

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

