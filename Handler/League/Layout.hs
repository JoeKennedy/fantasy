module Handler.League.Layout where

import Import

import Handler.Common       (extractValueMaybe)
import Handler.League.Setup

import Text.Blaze (toMarkup)

leagueLayout :: LeagueId -> Text -> Widget -> Handler Html
leagueLayout leagueId activeTab widget = do
    maybeUserId <- maybeAuthId
    league <- leagueOrRedirectIfIncomplete leagueId
    maybeTeam <- getCurrentTeam leagueId maybeUserId
    teams <- runDB $ selectList [TeamLeagueId ==. leagueId] [Asc TeamId]
    leagues <- getLeaguesByUser maybeUserId
    defaultLayout $ do
        setTitle $ toMarkup $ leagueLayoutTitle league activeTab
        $(widgetFile "layouts/league")

leagueLayoutTitle :: League -> Text -> Text
leagueLayoutTitle league subtitle = leagueName league ++ " | " ++ subtitle

getCurrentTeam :: LeagueId -> Maybe UserId -> Handler (Maybe (Entity Team))
getCurrentTeam leagueId maybeUserId =
    runDB $ selectFirst [TeamLeagueId ==. leagueId, TeamOwnerId ==. maybeUserId, TeamOwnerId !=. Nothing] []

---------------------
-- League Settings --
---------------------
leagueSettingsLayout :: LeagueId -> Route App -> Enctype -> Widget -> Text -> Handler Html
leagueSettingsLayout leagueId action enctype widget activePill = do
    maybeUserId <- maybeAuthId
    league <- runDB $ get404 leagueId
    maybeTeam <- getCurrentTeam leagueId maybeUserId
    let disableFields = disableSettingsFields maybeUserId (extractValueMaybe maybeTeam) league action
    leagueLayout leagueId "Settings" $ do
        $(widgetFile "layouts/settings")

isLeagueManager :: Maybe UserId -> League -> Bool
isLeagueManager (Just userId) league = userId == leagueCreatedBy league
isLeagueManager Nothing _ = False

canEnterDraftResults :: Maybe UserId -> League -> Bool
canEnterDraftResults maybeUserId league =
    isLeagueManager maybeUserId league && (not $ leagueIsDraftComplete league)

isTeamOwner :: Maybe UserId -> Maybe Team -> Bool
isTeamOwner (Just userId) (Just team) = Just userId == teamOwnerId team
isTeamOwner _ _ = False

disableSettingsFields :: Maybe UserId -> Maybe Team -> League -> Route App -> Bool
disableSettingsFields maybeUserId _ league (LeagueSettingsR _ LeagueTeamsSettingsR) =
    not $ isLeagueManager maybeUserId league
disableSettingsFields maybeUserId _ league (LeagueSettingsR _ LeagueEditSettingsR) =
    not $ isLeagueManager maybeUserId league
disableSettingsFields maybeUserId _ league (LeagueSettingsR _ _) =
    leagueIsDraftComplete league || (not $ isLeagueManager maybeUserId league)
disableSettingsFields maybeUserId maybeTeam _ (LeagueTeamSettingsR _ _) =
    not $ isTeamOwner maybeUserId maybeTeam
disableSettingsFields _ _ _ _ =
    error "Shouldn't be trying to disable fields for any other actions"

leagueOrRedirectIfIncomplete :: LeagueId -> Handler League
leagueOrRedirectIfIncomplete leagueId = do
    league <- runDB $ get404 leagueId
    if leagueIsSetupComplete league then return league else redirect $ leagueSetupNextStepToComplete league

