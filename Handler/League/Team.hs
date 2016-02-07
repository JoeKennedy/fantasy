module Handler.League.Team where

import Import

import Handler.League.Layout
import Handler.League.Setup

import Data.List  ((!!))
import Text.Blaze (toMarkup)

----------
-- Form --
----------
teamSettingsForm :: UserId -> League -> [Team] -> Html -> MForm Handler (FormResult [Team], Widget)
teamSettingsForm currentUserId league teams extra = do
    forms <- do
        fields <- forM teams (\team -> sequence $
            if leagueLastCompletedStep league > 4 then
                [ mreq textField  formControl (Just $ teamName team)
                , mreq textField  formControl (Just $ teamAbbreviation team)
                , mreq textField  formControl (Just $ teamOwnerName team)
                , mreq emailField formControl (Just $ teamOwnerEmail team)
                ]
            else
                [ mreq textField  (placeholder $ teamName team) Nothing
                , mreq textField  (placeholder $ teamAbbreviation team) Nothing
                , mreq textField  (placeholder $ teamOwnerName team) Nothing
                , mreq emailField (placeholder $ teamOwnerEmail team) Nothing
                ])
        return $ zip3 teams fields [1..leagueTeamsCount league]

    now <- liftIO getCurrentTime
    let teamSettingsResult = for forms (\(team, fields, _) -> Team
            <$> pure (teamLeagueId team)
            <*> fst (fields !! 0) -- teamNameRec
            <*> fst (fields !! 1) -- teamAbbreviationRec
            <*> pure (teamOwnerId team)
            <*> fst (fields !! 2) -- teamOwnerNameRec
            <*> fst (fields !! 3) -- teamOwnerEmailRec
            <*> pure (teamIsConfirmed team)
            <*> pure (teamCreatedBy team)
            <*> pure (teamCreatedAt team)
            <*> updatedByField currentUserId
            <*> pure now
            <*> pure (teamConfirmedBy team)
            <*> pure (teamConfirmedAt team))

    return (teamSettingsResult, $(widgetFile "league/team_settings_form"))

------------
-- Routes --
------------
getSetupTeamsSettingsR :: Handler Html
getSetupTeamsSettingsR = do
    userId <- requireAuthId
    (Entity leagueId league, lastCompletedStep) <- leagueOrRedirect userId
    teams <- runDB $ selectList [TeamLeagueId ==. leagueId] [Asc TeamId]
    (widget, enctype) <- generateFormPost $ teamSettingsForm userId league $ map extractValue teams
    defaultLayout $ do
        let action = SetupLeagueR SetupTeamsSettingsR
        setTitle $ leagueSetupStepTitle league action
        $(widgetFile "layouts/league-setup-layout")

postSetupTeamsSettingsR :: Handler Html
postSetupTeamsSettingsR = do
    userId <- requireAuthId
    (Entity leagueId league, lastCompletedStep) <- leagueOrRedirect userId
    teams <- runDB $ selectList [TeamLeagueId ==. leagueId] [Asc TeamId]
    ((result, widget), enctype) <- runFormPost $ teamSettingsForm userId league $ map extractValue teams
    case result of
        FormSuccess teams' -> do
            forM_ (zip teams teams') (\(Entity teamId _, team') -> runDB $ replace teamId team')
            updateLeagueLastCompletedStep leagueId league 5
            redirect $ SetupLeagueR SetupConfirmSettingsR
        _ -> defaultLayout $ do
            let action = SetupLeagueR SetupTeamsSettingsR
            setTitle $ leagueSetupStepTitle league action
            $(widgetFile "layouts/league-setup-layout")

getLeagueTeamsR :: LeagueId -> Handler Html
getLeagueTeamsR leagueId = do
    teams <- runDB $ selectList [TeamLeagueId ==. leagueId] [Asc TeamId]
    leagueLayout leagueId "Teams" $ do
        $(widgetFile "league/teams")

getLeagueTeamR :: LeagueId -> TeamId -> Handler Html
getLeagueTeamR leagueId teamId = do
    maybeUserId <- maybeAuthId
    team <- runDB $ get404 teamId
    let tab = if isUserTeamOwner maybeUserId team then "My Team" else "Teams"
    leagueLayout leagueId tab $ do
        $(widgetFile "league/team")

-- For now, just use the same form as all teams, just with one team
-- TODO - make a new form
getLeagueTeamSettingsR :: LeagueId -> TeamId -> Handler Html
getLeagueTeamSettingsR leagueId teamId = editTeamSettings leagueId (Just teamId) "My Team"

getLeagueTeamsSettingsR :: LeagueId -> Handler Html
getLeagueTeamsSettingsR leagueId = editTeamSettings leagueId Nothing "Teams"

postLeagueTeamSettingsR :: LeagueId -> TeamId -> Handler Html
postLeagueTeamSettingsR leagueId teamId = updateTeamSettings leagueId (Just teamId) "My Team"

postLeagueTeamsSettingsR :: LeagueId -> Handler Html
postLeagueTeamsSettingsR leagueId = updateTeamSettings leagueId Nothing "Teams"

-------------
-- Helpers --
-------------
isUserTeamOwner :: Maybe UserId -> Team -> Bool
isUserTeamOwner (Just userId) team = teamOwnerId team == Just userId
isUserTeamOwner _ _ = False

editTeamSettings :: LeagueId -> Maybe TeamId -> Text -> Handler Html
editTeamSettings leagueId maybeTeamId pillName = do
    userId <- requireAuthId
    league <- runDB $ get404 leagueId
    teams <- getTeamsForSettings leagueId maybeTeamId
    (widget, enctype) <- generateFormPost $ teamSettingsForm userId league $ map extractValue teams
    let action = teamSettingsAction leagueId maybeTeamId
    leagueSettingsLayout leagueId action enctype widget pillName

updateTeamSettings :: LeagueId -> Maybe TeamId -> Text -> Handler Html
updateTeamSettings leagueId maybeTeamId pillName = do
    userId <- requireAuthId
    league <- runDB $ get404 leagueId
    teams <- getTeamsForSettings leagueId maybeTeamId
    ((result, widget), enctype) <- runFormPost $ teamSettingsForm userId league $ map extractValue teams
    let action = teamSettingsAction leagueId maybeTeamId
    case result of
        FormSuccess teams' -> do
            forM_ (zip teams teams') (\(Entity teamId _, team') -> runDB $ replace teamId team')
            setMessage $ toMarkup $ "Successfully updated " ++ pillName ++ " settings"
            redirect action
        _ -> leagueSettingsLayout leagueId action enctype widget pillName

getTeamsForSettings :: (YesodPersist site, YesodPersistBackend site ~ SqlBackend) =>
                       LeagueId -> Maybe TeamId -> HandlerT site IO [Entity Team]
getTeamsForSettings leagueId Nothing = runDB $ selectList [TeamLeagueId ==. leagueId] [Asc TeamId]
getTeamsForSettings _ (Just teamId) = do
    team <- runDB $ get404 teamId
    return [Entity teamId team]

teamSettingsAction :: LeagueId -> Maybe TeamId -> Route App
teamSettingsAction leagueId (Just teamId) = LeagueTeamSettingsR leagueId teamId
teamSettingsAction leagueId Nothing = LeagueSettingsR leagueId LeagueTeamsSettingsR

