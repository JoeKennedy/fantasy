module Handler.League.Team where

import Import

import Handler.League.ConfirmSettings (sendJoinEmail)
import Handler.League.Layout
import Handler.League.Player      (getTeamPlayers, isLeagueMember,
                                   maybeAuthTeamId, playersModal,
                                   playersTable, playersWithButtons)
import Handler.League.Setup
import Handler.League.Transaction (getRequestedTransactions, getSuccessfulTransactions,
                                   transactionRequestsPanel, transactionsTable)
import Handler.League.Week        (FullPerformance, getPerformancesForGame)
import Handler.Score              (getGamesForTeam)

import Data.List  ((!!))
import Text.Blaze (toMarkup)

----------
-- Form --
----------
teamSettingsForm :: UserId -> League -> Season -> DraftSettings -> [Team] -> Form [Team]
teamSettingsForm currentUserId league season draftSettings teams extra = do
    let areTeamsSetup = leagueLastCompletedStep league > 4
        draftOrderType = draftSettingsDraftOrderType draftSettings
        isDraftOrderEditable = areTeamsSetup && draftOrderType == ManuallySet &&
                not (seasonIsDraftComplete season) && length teams > 1

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
            <*> pure (teamNumber team)
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
            <*> pure (teamPointsThisSeason team)
            <*> pure (teamPointsThisRegularSeason team)
            <*> pure (teamPointsThisPostSeason team)
            <*> pure (teamPostSeasonStatus team)
            <*> pure (teamCreatedBy team)
            <*> pure (teamCreatedAt team)
            <*> updatedByField currentUserId
            <*> pure now
            <*> pure (teamConfirmedBy team)
            <*> pure (teamConfirmedAt team)
            <*> pure (teamJoinEmailResentBy team)
            <*> pure (teamJoinEmailResentAt team))

    return (teamSettingsResult, $(widgetFile "league/team_settings_form"))


------------
-- Routes --
------------
getSetupTeamsSettingsR :: Handler Html
getSetupTeamsSettingsR = do
    userId <- requireAuthId
    let action = SetupLeagueR SetupTeamsSettingsR
    (Entity leagueId league, lastCompletedStep) <- leagueOrRedirect userId action
    teams <- runDB $ selectList [TeamLeagueId ==. leagueId] [Asc TeamNumber]
    Entity seasonId season <- getSelectedSeason leagueId
    Entity _ draftSettings <- runDB $ getBy404 $ UniqueDraftSettingsSeasonId seasonId
    (widget, enctype) <- generateFormPost $ teamSettingsForm userId league season draftSettings $ map entityVal teams
    defaultLayout $ do
        setTitle $ leagueSetupStepTitle league action
        let maybeLeagueId = Just leagueId
        $(widgetFile "layouts/league-setup-layout")

postSetupTeamsSettingsR :: Handler Html
postSetupTeamsSettingsR = do
    userId <- requireAuthId
    let action = SetupLeagueR SetupTeamsSettingsR
    (Entity leagueId league, lastCompletedStep) <- leagueOrRedirect userId action
    teams <- runDB $ selectList [TeamLeagueId ==. leagueId] [Asc TeamNumber]
    Entity seasonId season <- getSelectedSeason leagueId
    Entity _ draftSettings <- runDB $ getBy404 $ UniqueDraftSettingsSeasonId seasonId
    ((result, widget), enctype) <- runFormPost $ teamSettingsForm userId league season draftSettings $ map entityVal teams
    case result of
        FormSuccess teams' -> do
            replaceTeams userId seasonId teams teams'
            updateLeagueLastCompletedStep leagueId league 5
            redirect $ SetupLeagueR SetupConfirmSettingsR
        _ -> defaultLayout $ do
            setTitle $ leagueSetupStepTitle league action
            let maybeLeagueId = Just leagueId
            $(widgetFile "layouts/league-setup-layout")

getLeagueTeamsR :: LeagueId -> Handler Html
getLeagueTeamsR leagueId = do
    maybeUserId <- maybeAuthId
    league <- runDB $ get404 leagueId
    Entity seasonId season <- getSelectedSeason leagueId
    teams <- getTeamsOrderBy seasonId False TeamSeasonRegularSeasonPoints
    withResendable <- mapM (addIsJoinEmailResendable maybeUserId league) teams
    let fullTeams = rank3 withResendable
    leagueLayout leagueId "Houses" $(widgetFile "league/teams")

getLeagueTeamR :: LeagueId -> Int -> Handler Html
getLeagueTeamR leagueId number = do
    maybeUserId <- maybeAuthId
    Entity seasonId season <- getSelectedSeason leagueId
    isUserLeagueMember <- isLeagueMember maybeUserId leagueId
    maybeUserTeamId <- maybeAuthTeamId leagueId
    league <- runDB $ get404 leagueId
    Entity teamId team <- runDB $ getBy404 $ UniqueTeamLeagueIdNumber leagueId number
    joinEmailResendable <- isJoinEmailResendable maybeUserId league $ Entity teamId team
    teamPlayers <- getTeamPlayers seasonId $ Just teamId
    players <- playersWithButtons leagueId season teamPlayers
    Entity _ generalSettings <- runDB $ getBy404 $ UniqueGeneralSettingsSeasonId seasonId 
    transactions <- getSuccessfulTransactions seasonId (Just teamId) Nothing
    tradeProposals <- getRequestedTransactions seasonId (Just teamId) Trade
    waiverClaims <- getRequestedTransactions seasonId (Just teamId) Claim
    currentTeamPlayers <- getTeamPlayers seasonId maybeUserTeamId
    myPlayers <- playersWithButtons leagueId season currentTeamPlayers
    weeks <- runDB $ selectList [WeekLeagueId ==. leagueId] [Asc WeekNumber]
    games <- getGamesForTeam seasonId teamId
    performances <- mapM (getPerformancesForGame . fst) games
    Entity _ teamSeason <- runDB $ getBy404 $ UniqueTeamSeasonTeamIdSeasonId teamId seasonId
    let gamesAndPerformances = zip games performances
        tab = if isUserTeamOwner maybeUserId team then "My House" else "Houses"
        numberOfStarters = generalSettingsNumberOfStarters generalSettings
        rosterSize = generalSettingsRosterSize generalSettings
    leagueLayout leagueId tab $(widgetFile "league/team")

-- For now, just use the same form as all teams, just with one team
-- TODO - make a new form
getLeagueTeamSettingsR :: LeagueId -> Int -> Handler Html
getLeagueTeamSettingsR leagueId number = editTeamSettings leagueId (Just number) "My House"

getLeagueTeamsSettingsR :: LeagueId -> Handler Html
getLeagueTeamsSettingsR leagueId = editTeamSettings leagueId Nothing "Houses"

postLeagueTeamSettingsR :: LeagueId -> Int -> Handler Html
postLeagueTeamSettingsR leagueId number = updateTeamSettings leagueId (Just number) "My House"

postLeagueTeamsSettingsR :: LeagueId -> Handler Html
postLeagueTeamsSettingsR leagueId = updateTeamSettings leagueId Nothing "Houses"

getLeagueTeamJoinR :: LeagueId -> Int -> Text -> Handler Html
getLeagueTeamJoinR leagueId number verificationKey = do
    maybeUserId <- maybeAuthId
    league <- runDB $ get404 leagueId
    Entity _ team <- runDB $ getBy404 $ UniqueTeamLeagueIdNumber leagueId number
    maybeLeagueManagerTeam <- runDB $ selectFirst [TeamLeagueId ==. leagueId] [Asc TeamNumber]
    defaultLayout $ do
        setTitle $ toMarkup $ "Join League " ++ leagueName league
        $(widgetFile "league/join")

postLeagueTeamJoinR :: LeagueId -> Int -> Text -> Handler Html
postLeagueTeamJoinR leagueId number _verificationKey = do
    userId <- requireAuthId
    now <- liftIO getCurrentTime
    Entity teamId _ <- runDB $ getBy404 $ UniqueTeamLeagueIdNumber leagueId number
    runDB $ update teamId [ TeamIsConfirmed =. True
                          , TeamOwnerId =. Just userId
                          , TeamUpdatedBy =. userId
                          , TeamUpdatedAt =. now
                          , TeamConfirmedBy =. Just userId
                          , TeamConfirmedAt =. Just now
                          ]
    -- TODO - send an email to the owner who just joined
    setMessage "Congrats! You've successfully joined the league!"
    redirect $ LeagueTeamR leagueId number

postLeagueTeamResendR :: LeagueId -> Int -> Handler ()
postLeagueTeamResendR leagueId number = do
    userId <- requireAuthId
    league <- runDB $ get404 leagueId
    Entity teamId team <- runDB $ getBy404 $ UniqueTeamLeagueIdNumber leagueId number
    leagueManagerTeam <- runDB $ selectFirst [TeamLeagueId ==. leagueId] [Asc TeamNumber]
    case leagueManagerTeam of
        Nothing -> return ()
        Just managerTeam -> do
            sendJoinEmail (Entity leagueId league) managerTeam $ Entity teamId team
            now <- liftIO getCurrentTime
            runDB $ update teamId [ TeamJoinEmailResentBy =. Just userId
                                  , TeamJoinEmailResentAt =. Just now
                                  , TeamUpdatedBy =. userId
                                  , TeamUpdatedAt =. now
                                  ]
            setMessage $ toMarkup $ "Resent join email for House " ++ teamName team ++ " to " ++
                        teamOwnerName team ++ " at " ++ teamOwnerEmail team ++ "."


-------------
-- Widgets --
-------------
teamPerformancesTable :: [FullPerformance] -> Rational -> Int -> Int -> Widget
teamPerformancesTable performances totalPoints numberOfStarters rosterSize =
    let colspan = 3 :: Int
        (starters, bench) = partition (\(Entity _ p, _, _, _, _) -> performanceIsStarter p) performances
        fullStarters = zipWith (\n (a,b,c,d,e) -> (show n,a,b,c,d,e)) ([1..] :: [Int]) starters
        fullBench    = map (\(a,b,c,d,e) -> ("Bench" :: String,a,b,c,d,e)) bench
        extraStarterSlots = map show            [length fullStarters + 1 .. numberOfStarters]
        extraBenchSlots   = map (\_ -> "Bench") [length fullBench + 1 .. rosterSize - numberOfStarters]
        splitPerformances = [ (("Starters" :: Text), fullStarters, extraStarterSlots, Just totalPoints)
                            , ("Bench", fullBench, extraBenchSlots, Nothing)
                            ]
    in  $(widgetFile "league/performances_table")

resendJoinButton :: LeagueId -> Int -> Widget
resendJoinButton leagueId number =
    let buttonId = "resend-" ++ toPathPiece leagueId ++ "-" ++ (pack $ show number)
    in  $(widgetFile "league/resend_join_button")


-------------
-- Helpers --
-------------
isUserTeamOwner :: Maybe UserId -> Team -> Bool
isUserTeamOwner (Just userId) team = teamOwnerId team == Just userId
isUserTeamOwner _ _ = False

editTeamSettings :: LeagueId -> Maybe Int -> Text -> Handler Html
editTeamSettings leagueId maybeTeamNumber pillName = do
    userId <- requireAuthId
    league <- runDB $ get404 leagueId
    teams <- getTeamsForSettings leagueId maybeTeamNumber
    Entity seasonId season <- getSelectedSeason leagueId
    Entity _ draftSettings <- runDB $ getBy404 $ UniqueDraftSettingsSeasonId seasonId
    (widget, enctype) <- generateFormPost $ teamSettingsForm userId league season draftSettings $ map entityVal teams
    let action = teamSettingsAction leagueId maybeTeamNumber
    leagueSettingsLayout leagueId action enctype widget pillName

updateTeamSettings :: LeagueId -> Maybe Int -> Text -> Handler Html
updateTeamSettings leagueId maybeTeamNumber pillName = do
    userId <- requireAuthId
    league <- runDB $ get404 leagueId
    teams <- getTeamsForSettings leagueId maybeTeamNumber
    Entity seasonId season <- getSelectedSeason leagueId
    Entity _ draftSettings <- runDB $ getBy404 $ UniqueDraftSettingsSeasonId seasonId
    ((result, widget), enctype) <- runFormPost $ teamSettingsForm userId league season draftSettings $ map entityVal teams
    let action = teamSettingsAction leagueId maybeTeamNumber
    case result of
        FormSuccess teams' -> do
            replaceTeams userId seasonId teams teams'
            setMessage $ toMarkup $ "Successfully updated " ++ pillName ++ " settings"
            redirect action
        _ -> leagueSettingsLayout leagueId action enctype widget pillName

replaceTeams :: UserId -> SeasonId -> [Entity Team] -> [Team] -> Handler ()
replaceTeams userId seasonId teams teams' =
    mapM_ (replaceTeam userId seasonId) (zip (map entityKey teams) teams')

replaceTeam :: UserId -> SeasonId -> (TeamId, Team) -> Handler ()
replaceTeam userId seasonId (teamId, team') = runDB $ do
    replace teamId team'
    now <- liftIO getCurrentTime
    updateWhere [ TeamSeasonTeamId ==. teamId, TeamSeasonSeasonId ==. seasonId ]
                [ TeamSeasonDraftOrder =. teamDraftOrder team'
                , TeamSeasonUpdatedBy  =. userId
                , TeamSeasonUpdatedAt  =. now ]

getTeamsForSettings :: LeagueId -> Maybe Int -> Handler [Entity Team]
getTeamsForSettings leagueId Nothing = runDB $ selectList [TeamLeagueId ==. leagueId] [Asc TeamNumber]
getTeamsForSettings leagueId (Just number) = do
    teamEntity <- runDB $ getBy404 $ UniqueTeamLeagueIdNumber leagueId number
    return [teamEntity]

teamSettingsAction :: LeagueId -> Maybe Int -> Route App
teamSettingsAction leagueId (Just number) = LeagueTeamSettingsR leagueId number
teamSettingsAction leagueId Nothing = LeagueSettingsR leagueId LeagueTeamsSettingsR

isJoinEmailResendable :: Maybe UserId -> League -> Entity Team -> Handler Bool
isJoinEmailResendable Nothing _ _ = return False
isJoinEmailResendable (Just userId) league (Entity _ team) = do
    emailSentRecently <- liftIO $ past24Hours $ teamJoinEmailResentAt team
    -- now <- liftIO getCurrentTime
    -- let resend = fromMaybe now $ teamJoinEmailResentAt team
    -- $(logInfo) (pack $ show emailSentRecently)
    -- $(logInfo) ()
    return $ isLeagueManager (Just userId) league && not emailSentRecently

addIsJoinEmailResendable :: Maybe UserId -> League -> (Entity Team, Entity TeamSeason) ->
                            Handler (Entity Team, Entity TeamSeason, Bool)
addIsJoinEmailResendable maybeUserId league (teamEntity, teamSeasonEntity) = do
    joinEmailResendable <- isJoinEmailResendable maybeUserId league teamEntity
    return (teamEntity, teamSeasonEntity, joinEmailResendable)
