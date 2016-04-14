module Handler.League.Team where

import Import

import Handler.Common             (extractValue)
import Handler.League.Layout
import Handler.League.Player      (getTeamPlayers, isLeagueMember,
                                   maybeAuthTeamId, playersModal,
                                   playersTable, playersWithButtons)
import Handler.League.Transaction (getRequestedTransactions, getSuccessfulTransactions,
                                   transactionRequestsPanel, transactionsTable)
import Handler.League.Setup

import Data.List  ((!!))
import Data.Maybe (fromJust)
import Text.Blaze (toMarkup)

----------
-- Form --
----------
teamSettingsForm :: UserId -> League -> DraftSettings -> [Team] -> Form [Team]
teamSettingsForm currentUserId league draftSettings teams extra = do
    let areTeamsSetup = leagueLastCompletedStep league > 4
        draftOrderType = draftSettingsDraftOrderType draftSettings
        isDraftOrderEditable = areTeamsSetup && draftOrderType == ManuallySet &&
                not (leagueIsDraftComplete league) && length teams > 1

    forms <- do
        draftOrderFields <- forM teams (\team ->
            mreq intField formControl (Just $ teamDraftOrder team))
        textFields <- forM teams (\team -> sequence $
            if areTeamsSetup then
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
        return $ zip4 teams textFields draftOrderFields [1..leagueTeamsCount league]

    now <- liftIO getCurrentTime
    let showEmail = case teams of []    -> False
                                  (t:_) -> Just currentUserId == teamOwnerId t
        teamSettingsResult = for forms (\(team, textFields, draftOrderField, _) -> Team
            <$> pure (teamLeagueId team)
            <*> fst (textFields !! 0) -- teamNameRec
            <*> fst (textFields !! 1) -- teamAbbreviationRec
            <*> pure (teamOwnerId team)
            <*> fst (textFields !! 2) -- teamOwnerNameRec
            <*> fst (textFields !! 3) -- teamOwnerEmailRec
            <*> pure (teamIsConfirmed team)
            <*> pure (teamPlayersCount team)
            <*> pure (teamStartersCount team)
            <*> (if isDraftOrderEditable
                    then fst draftOrderField
                    else pure (teamDraftOrder team))
            <*> pure (teamWaiverOrder team)
            <*> pure (teamVerificationKey team)
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
    let action = SetupLeagueR SetupTeamsSettingsR
    (Entity leagueId league, lastCompletedStep) <- leagueOrRedirect userId action
    teams <- runDB $ selectList [TeamLeagueId ==. leagueId] [Asc TeamId]
    Entity _ draftSettings <- runDB $ getBy404 $ UniqueDraftSettingsLeagueId leagueId
    (widget, enctype) <- generateFormPost $ teamSettingsForm userId league draftSettings $ map extractValue teams
    defaultLayout $ do
        setTitle $ leagueSetupStepTitle league action
        let maybeLeagueId = Just leagueId
        $(widgetFile "layouts/league-setup-layout")

postSetupTeamsSettingsR :: Handler Html
postSetupTeamsSettingsR = do
    userId <- requireAuthId
    let action = SetupLeagueR SetupTeamsSettingsR
    (Entity leagueId league, lastCompletedStep) <- leagueOrRedirect userId action
    teams <- runDB $ selectList [TeamLeagueId ==. leagueId] [Asc TeamId]
    Entity _ draftSettings <- runDB $ getBy404 $ UniqueDraftSettingsLeagueId leagueId
    ((result, widget), enctype) <- runFormPost $ teamSettingsForm userId league draftSettings $ map extractValue teams
    case result of
        FormSuccess teams' -> do
            forM_ (zip teams teams') (\(Entity teamId _, team') -> runDB $ replace teamId team')
            updateLeagueLastCompletedStep leagueId league 5
            redirect $ SetupLeagueR SetupConfirmSettingsR
        _ -> defaultLayout $ do
            setTitle $ leagueSetupStepTitle league action
            let maybeLeagueId = Just leagueId
            $(widgetFile "layouts/league-setup-layout")

getLeagueTeamsR :: LeagueId -> Handler Html
getLeagueTeamsR leagueId = do
    maybeUserId <- maybeAuthId
    isUserLeagueMember <- isLeagueMember maybeUserId leagueId
    league <- runDB $ get404 leagueId
    teams <- runDB $ selectList [TeamLeagueId ==. leagueId] [Asc TeamId]
    leagueLayout leagueId "Teams" $ do
        $(widgetFile "league/teams")

getLeagueTeamR :: LeagueId -> TeamId -> Handler Html
getLeagueTeamR leagueId teamId = do
    maybeUserId <- maybeAuthId
    isUserLeagueMember <- isLeagueMember maybeUserId leagueId
    maybeUserTeamId <- maybeAuthTeamId leagueId
    league <- runDB $ get404 leagueId
    let leagueEntity = Entity leagueId league
    team <- runDB $ get404 teamId
    teamPlayers <- getTeamPlayers $ Just teamId
    players <- playersWithButtons leagueEntity teamPlayers
    Entity _ generalSettings <- runDB $ getBy404 $ UniqueGeneralSettingsLeagueId leagueId
    transactions <- getSuccessfulTransactions leagueId (Just teamId) Nothing
    tradeProposals <- getRequestedTransactions leagueId (Just teamId) Trade
    waiverClaims <- getRequestedTransactions leagueId (Just teamId) Claim
    currentTeamPlayers <- getTeamPlayers maybeUserTeamId
    myPlayers <- playersWithButtons leagueEntity currentTeamPlayers
    let tab = if isUserTeamOwner maybeUserId team then "My Team" else "Teams"
        numberOfStarters = generalSettingsNumberOfStarters generalSettings
        rosterSize = generalSettingsRosterSize generalSettings
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

getLeagueTeamJoinR :: LeagueId -> TeamId -> Text -> Handler Html
getLeagueTeamJoinR leagueId teamId verificationKey = do
    maybeUserId <- maybeAuthId
    league <- runDB $ get404 leagueId
    team <- runDB $ get404 teamId
    maybeLeagueManagerTeam <- runDB $ selectFirst [ TeamLeagueId ==. leagueId
                                                  , TeamOwnerId ==. Just (leagueCreatedBy league)
                                                  ] []
    let (Entity _ leagueManagerTeam) = fromJust maybeLeagueManagerTeam
    defaultLayout $ do
        setTitle $ toMarkup $ "Join League " ++ leagueName league
        $(widgetFile "league/join")

postLeagueTeamJoinR :: LeagueId -> TeamId -> Text -> Handler Html
postLeagueTeamJoinR leagueId teamId _verificationKey = do
    userId <- requireAuthId
    now <- liftIO getCurrentTime
    runDB $ update teamId [ TeamIsConfirmed =. True
                          , TeamOwnerId =. Just userId
                          , TeamUpdatedBy =. userId
                          , TeamUpdatedAt =. now
                          , TeamConfirmedBy =. Just userId
                          , TeamConfirmedAt =. Just now
                          ]
    -- TODO - send an email to the owner who just joined
    setMessage "Congrats! You've successfully joined the league!"
    redirect $ LeagueTeamR leagueId teamId

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
    Entity _ draftSettings <- runDB $ getBy404 $ UniqueDraftSettingsLeagueId leagueId
    (widget, enctype) <- generateFormPost $ teamSettingsForm userId league draftSettings $ map extractValue teams
    let action = teamSettingsAction leagueId maybeTeamId
    leagueSettingsLayout leagueId action enctype widget pillName

updateTeamSettings :: LeagueId -> Maybe TeamId -> Text -> Handler Html
updateTeamSettings leagueId maybeTeamId pillName = do
    userId <- requireAuthId
    league <- runDB $ get404 leagueId
    teams <- getTeamsForSettings leagueId maybeTeamId
    Entity _ draftSettings <- runDB $ getBy404 $ UniqueDraftSettingsLeagueId leagueId
    ((result, widget), enctype) <- runFormPost $ teamSettingsForm userId league draftSettings $ map extractValue teams
    let action = teamSettingsAction leagueId maybeTeamId
    case result of
        FormSuccess teams' -> do
            forM_ (zip teams teams') (\(Entity teamId _, team') -> runDB $ replace teamId team')
            setMessage $ toMarkup $ "Successfully updated " ++ pillName ++ " settings"
            redirect action
        _ -> leagueSettingsLayout leagueId action enctype widget pillName

getTeamsForSettings :: LeagueId -> Maybe TeamId -> Handler [Entity Team]
getTeamsForSettings leagueId Nothing = runDB $ selectList [TeamLeagueId ==. leagueId] [Asc TeamId]
getTeamsForSettings _ (Just teamId) = do
    team <- runDB $ get404 teamId
    return [Entity teamId team]

teamSettingsAction :: LeagueId -> Maybe TeamId -> Route App
teamSettingsAction leagueId (Just teamId) = LeagueTeamSettingsR leagueId teamId
teamSettingsAction leagueId Nothing = LeagueSettingsR leagueId LeagueTeamsSettingsR

