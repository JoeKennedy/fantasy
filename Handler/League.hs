module Handler.League where

import Import
import Handler.Common        (extractValueMaybe)
import Handler.League.Setup
import Handler.League.Layout

----------
-- Form --
----------
leagueForm :: UserId -> Maybe League -> Html -> MForm Handler (FormResult League, Widget)
leagueForm currentUserId league extra = do
    (nameRes, nameView) <- mreq textField (fieldName "Name") (leagueName <$> league)
    (isPrivateRes, isPrivateView) <- mreq checkBoxField "Is league private?"
        (leagueIsPrivate <$> league)
    (scoringTypeRes, scoringTypeView) <- mreq hiddenField (hidden "Scoring type")
        (leagueScoringType <$> league)
    (teamsCountRes, teamsCountView) <- mreq (selectFieldList teamsCountOptions)
        (fieldName "Number of teams") (leagueTeamsCount <$> league)

    now <- liftIO getCurrentTime
    let leagueResult = League
            <$> nameRes
            <*> isPrivateRes
            <*> scoringTypeRes
            <*> teamsCountRes
            <*> existingElseDefault False (leagueIsSetupComplete <$> league)
            <*> existingElseDefault 1 (leagueLastCompletedStep <$> league)
            <*> createdByField currentUserId (leagueCreatedBy <$> league)
            <*> existingElseDefault now (leagueCreatedAt <$> league)
            <*> updatedByField currentUserId
            <*> pure now
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

getSetupNewLeagueR :: Handler Html
getSetupNewLeagueR = do
    userId <- requireAuthId
    maybeLeague <- leagueBeingSetUp userId
    (widget, enctype) <- generateFormPost $ leagueForm userId $ extractValueMaybe maybeLeague
    defaultLayout $ do
        let title = "Create A League!" :: Html
            action = SetupLeagueR SetupNewLeagueR
            lastCompletedStep = fromMaybe 0 (leagueLastCompletedStep <$> extractValueMaybe maybeLeague)
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
            setTitle title
            $(widgetFile "layouts/league-setup-layout")

getLeagueR :: LeagueId -> Handler Html
getLeagueR leagueId = do
    league <- runDB $ get404 leagueId
    teams <- runDB $ selectList [TeamLeagueId ==. leagueId] [Asc TeamId]
    leagueLayout leagueId "League" $ do
        let maybeCreatorTeam = listToMaybe teams
        $(widgetFile "league/league")

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
createLeague :: (YesodPersist site, YesodPersistBackend site ~ SqlBackend) =>
                League -> HandlerT site IO ()
createLeague league = runDB $ do
    leagueId <- insert league
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
    createFirstTeam leagueEntity
    mapM_ (createTeam leagueEntity) [2..(leagueTeamsCount league)]
    characters <- selectKeysList [] [Asc CharacterName]
    mapM_ (createPlayer leagueEntity) characters
    return ()

createScoringSettingsRow :: (MonadIO m) => Entity League -> Action -> ReaderT SqlBackend m ()
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

createFirstTeam :: (MonadIO m) => Entity League -> ReaderT SqlBackend m ()
createFirstTeam (Entity leagueId league) =
    insert_ $ Team { teamLeagueId      = leagueId
                   , teamName          = "Number 1"
                   , teamAbbreviation  = "N1"
                   , teamOwnerId       = Just $ leagueCreatedBy league
                   , teamOwnerName     = "Owner 1"
                   , teamOwnerEmail    = "Enter your email"
                   , teamIsConfirmed   = True
                   , teamPlayersCount  = 0
                   , teamStartersCount = 0
                   , teamCreatedBy     = leagueCreatedBy league
                   , teamCreatedAt     = leagueCreatedAt league
                   , teamUpdatedBy     = leagueUpdatedBy league
                   , teamUpdatedAt     = leagueUpdatedAt league
                   , teamConfirmedBy   = Just $ leagueCreatedBy league
                   , teamConfirmedAt   = Just $ leagueCreatedAt league
                   }

createTeam :: (MonadIO m) => Entity League -> Int -> ReaderT SqlBackend m ()
createTeam (Entity leagueId league) int =
    insert_ $ Team { teamLeagueId      = leagueId
                   , teamName          = pack $ "Number " ++ show int
                   , teamAbbreviation  = pack $ "N" ++ show int
                   , teamOwnerId       = Nothing
                   , teamOwnerName     = pack $ "Owner " ++ show int
                   , teamOwnerEmail    = pack $ "Enter Team " ++ show int ++ "'s email"
                   , teamIsConfirmed   = False
                   , teamPlayersCount  = 0
                   , teamStartersCount = 0
                   , teamCreatedBy     = leagueCreatedBy league
                   , teamCreatedAt     = leagueCreatedAt league
                   , teamUpdatedBy     = leagueUpdatedBy league
                   , teamUpdatedAt     = leagueUpdatedAt league
                   , teamConfirmedBy   = Nothing
                   , teamConfirmedAt   = Nothing
                   }

createPlayer :: (MonadIO m) => Entity League -> CharacterId -> ReaderT SqlBackend m ()
createPlayer (Entity leagueId league) characterId =
    insert_ $ Player { playerLeagueId    = leagueId
                     , playerCharacterId = characterId
                     , playerTeamId      = Nothing
                     , playerIsStarter   = False
                     , playerCreatedBy   = leagueCreatedBy league
                     , playerCreatedAt   = leagueCreatedAt league
                     , playerUpdatedBy   = leagueUpdatedBy league
                     , playerUpdatedAt   = leagueUpdatedAt league
                     }

-------------
-- Helpers --
-------------
scoringTypeWidget :: (MonadIO m, MonadBaseControl IO m, MonadThrow m) =>
                     ScoringType -> WidgetT site m ()
scoringTypeWidget scoringType = $(widgetFile "league/scoring_type")

leagueListGroupItem :: (MonadIO m, MonadBaseControl IO m, MonadThrow m) =>
                       Maybe League -> ScoringType -> WidgetT site m ()
leagueListGroupItem (Just league) scoringType
    | leagueScoringType league == scoringType =
        [whamlet|<div .list-group-item .active>^{scoringTypeWidget scoringType}|]
    | otherwise = [whamlet|<div .list-group-item>^{scoringTypeWidget scoringType}|]
leagueListGroupItem Nothing scoringType =
    [whamlet|<a .list-group-item href="#">^{scoringTypeWidget scoringType}|]

