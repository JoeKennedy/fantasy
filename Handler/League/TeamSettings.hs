module Handler.League.TeamSettings where

import Import
import Handler.League.Setup

import Data.List ((!!))

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
getSetupTeamSettingsR :: Handler Html
getSetupTeamSettingsR = do
    userId <- requireAuthId
    (Entity leagueId league, lastCompletedStep) <- leagueOrRedirect userId
    teams <- runDB $ selectList [TeamLeagueId ==. leagueId] []
    (widget, enctype) <- generateFormPost $ teamSettingsForm userId league $ map extractValue teams
    defaultLayout $ do
        let action = SetupLeagueR SetupTeamSettingsR
        setTitle $ leagueSetupStepTitle league action
        $(widgetFile "layouts/league-setup-layout")

postSetupTeamSettingsR :: Handler Html
postSetupTeamSettingsR = do
    userId <- requireAuthId
    (Entity leagueId league, lastCompletedStep) <- leagueOrRedirect userId
    teams <- runDB $ selectList [TeamLeagueId ==. leagueId] []
    ((result, widget), enctype) <- runFormPost $ teamSettingsForm userId league $ map extractValue teams
    case result of
        FormSuccess teams' -> do
            forM_ (zip teams teams') (\(Entity teamId _, team') -> runDB $ replace teamId team')
            updateLeagueLastCompletedStep leagueId league 5
            redirect $ SetupLeagueR SetupConfirmSettingsR
        _ -> defaultLayout $ do
            let action = SetupLeagueR SetupTeamSettingsR
            setTitle $ leagueSetupStepTitle league action
            $(widgetFile "layouts/league-setup-layout")

