module Handler.League where

import Import

import Handler.League.Layout
import Handler.League.Season (createLeagueSeason)
import Handler.League.Setup

import Network.Mail.Mime
import System.Random     (newStdGen)

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
    seasonId <- getSelectedSeasonId leagueId
    teams <- getTeamsOrderBy seasonId False TeamSeasonRegularSeasonPoints
    let maybeCreatorTeam = find (\(Entity _ t, _) -> teamOwnerId t == Just (leagueCreatedBy league)) teams
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
    runDB $ updateWhere [ SeasonLeagueId ==. leagueId ]
                        [ SeasonIsActive  =. False
                        , SeasonUpdatedBy =. userId
                        , SeasonUpdatedAt =. now
                        ]
    setMessage "Your league has been successfully canceled"

getSetupNewLeagueR :: Handler Html
getSetupNewLeagueR = do
    userId <- requireAuthId
    maybeLeague <- leagueBeingSetUp userId
    (widget, enctype) <- generateFormPost $ leagueForm userId $ map entityVal maybeLeague
    defaultLayout $ do
        let title = "Create A League!" :: Html
            action = SetupLeagueR SetupNewLeagueR
            lastCompletedStep = fromMaybe 0 (leagueLastCompletedStep <$> map entityVal maybeLeague)
            maybeLeagueId = map entityKey maybeLeague
        setTitle title
        $(widgetFile "layouts/league-setup-layout")

postSetupNewLeagueR :: Handler Html
postSetupNewLeagueR = do
    userId <- requireAuthId
    maybeLeague <- leagueBeingSetUp userId
    ((result, widget), enctype) <- runFormPost $ leagueForm userId $ map entityVal maybeLeague
    case result of
        FormSuccess league -> do
            case maybeLeague of Just (Entity lId _) -> runDB $ replace lId league
                                Nothing             -> createLeague league
            redirect $ SetupLeagueR SetupGeneralSettingsR
        _ -> defaultLayout $ do
            let title = "Create A League!" :: Html
                action = SetupLeagueR SetupNewLeagueR
                lastCompletedStep = fromMaybe 0 (leagueLastCompletedStep <$> map entityVal maybeLeague)
                maybeLeagueId = map entityKey maybeLeague
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
    leagueId <- runDB $ insert league
    seriesList <- runDB $ selectList [SeriesNumber >=. 6] [Desc SeriesNumber]
    let userId = leagueCreatedBy league
    mapM_ (\s -> createLeagueSeason userId s $ Entity leagueId league) seriesList


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

