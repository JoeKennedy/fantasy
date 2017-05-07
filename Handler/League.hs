module Handler.League where

import Import
import Handler.Common             (backgroundHandler, extractKeyMaybe, extractValueMaybe)
import Handler.League.Setup
import Handler.League.Layout
import Handler.League.Week        (createWeekData_)
import Handler.League.Transaction (cancelAllTrades, cancelAllTransactionRequests)
import Handler.Score              (finalizeWeek, createPlay)

import Data.Maybe                   (fromJust)
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
            <*> existingElseDefault False (leagueIsSeasonComplete <$> league)
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
    let leagueEntity = Entity leagueId league
    userId <- requireAuthId
    runDB $ do
        createGeneralSettings leagueEntity
        mapM_ (createScoringSettingsRow leagueEntity) allActions
        mapM_ (createTeam leagueEntity) $ zip teamNumbers draftOrder

    -- create players and previous weeks in background; they aren't needed for setup
    backgroundHandler $ do
        characters <- runDB $ selectList [] [Asc CharacterName]
        runDB $ mapM_ (createPlayer leagueEntity) characters
        -- create week and related data for any already aired episodes this season
        maybeSeries <- runDB $ selectFirst [] [Desc SeriesNumber]
        case maybeSeries of
            Nothing -> return ()
            Just (Entity seriesId _) -> do
                episodes <- runDB $ selectList [ EpisodeSeriesId ==. seriesId
                                               , EpisodeStatus !=. YetToAir
                                               ] [Asc EpisodeId]
                mapM_ (backfillWeekData leagueId userId) episodes

createGeneralSettings :: Entity League -> ReaderT SqlBackend Handler ()
createGeneralSettings (Entity leagueId league) = do
    let teamsCount = leagueTeamsCount league
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
createTeam (Entity leagueId league) (number, draftOrder) = do
    stdgen <- liftIO newStdGen
    let (name, abbrev, owner, email) = teamTextAttributes number
        (maybeTeamOwnerId, maybeConfirmedAt) = teamNonTextAttributes league number
        verificationKey = pack $ fst $ randomString 24 stdgen
    insert_ $ Team { teamLeagueId         = leagueId
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
                   , teamCreatedBy        = leagueCreatedBy league
                   , teamCreatedAt        = leagueCreatedAt league
                   , teamUpdatedBy        = leagueUpdatedBy league
                   , teamUpdatedAt        = leagueUpdatedAt league
                   , teamConfirmedBy      = maybeTeamOwnerId
                   , teamConfirmedAt      = maybeConfirmedAt
                   , teamJoinEmailResentBy = Nothing
                   , teamJoinEmailResentAt = Nothing
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


--------------------
-- Trade Deadline --
--------------------
determineIfTradeDeadlineHasPassed :: Int -> LeagueId -> Handler ()
determineIfTradeDeadlineHasPassed weekNo leagueId = do
    league <- runDB $ get404 leagueId
    if leagueIsAfterTradeDeadline league then return () else do
        isBeforeTradeDeadline <- weekNumberBeforeTradeDeadline leagueId weekNo
        if isBeforeTradeDeadline then return () else do
            maybeAdmin <- runDB $ selectFirst [UserIsAdmin ==. True] [Asc UserId]
            let Entity adminUserId _ = fromJust maybeAdmin
            now <- liftIO getCurrentTime
            runDB $ update leagueId [ LeagueIsAfterTradeDeadline =. True
                                    , LeagueUpdatedBy =. adminUserId
                                    , LeagueUpdatedAt =. now
                                    ]
            cancelAllTrades adminUserId leagueId

weekNumberBeforeTradeDeadline :: LeagueId -> Int -> Handler Bool
weekNumberBeforeTradeDeadline leagueId weekNo = do
    Entity _ generalSettings <- runDB $ getBy404 $ UniqueGeneralSettingsLeagueId leagueId
    return $ weekNo < generalSettingsTradeDeadlineWeek generalSettings


---------------------
-- Complete Season --
---------------------
completeSeason :: Int -> LeagueId -> Handler ()
completeSeason weekNo leagueId = do
    if weekNo /= 7 then return () else do
        maybeAdmin <- runDB $ selectFirst [UserIsAdmin ==. True] [Asc UserId]
        let Entity adminUserId _ = fromJust maybeAdmin
        now <- liftIO getCurrentTime
        runDB $ update leagueId [ LeagueIsSeasonComplete =. True
                                , LeagueUpdatedBy =. adminUserId
                                , LeagueUpdatedAt =. now
                                ]
        cancelAllTransactionRequests adminUserId leagueId


------------------------
-- Backfill Week Data --
------------------------
backfillWeekData :: LeagueId -> UserId -> Entity Episode -> Handler ()
backfillWeekData leagueId userId (Entity episodeId episode) = do
    createWeekData_ (Entity episodeId episode) leagueId
    now <- liftIO getCurrentTime
    league <- runDB $ get404 leagueId
    events <- runDB $ selectList [EventEpisodeId ==. episodeId] [Asc EventTimeInEpisode]
    Entity weekId _ <- runDB $ getBy404 $ UniqueWeekLeagueIdEpisodeId leagueId episodeId
    forM_ events $ createPlay leagueId weekId
    finalizeWeek episodeId userId now $ Entity leagueId league

