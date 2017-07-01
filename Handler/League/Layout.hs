module Handler.League.Layout where

import Import

import Handler.League.Setup

import qualified Database.Esqueleto as E
import           Database.Esqueleto ((^.))
import           Text.Blaze         (toMarkup)

------------
-- Layout --
------------
leagueLayout :: LeagueId -> Text -> Widget -> Handler Html
leagueLayout leagueId activeTab widget = do
    maybeUserId <- maybeAuthId
    league <- leagueOrRedirectIfIncomplete leagueId
    maybeTeam <- getCurrentTeam leagueId maybeUserId
    teams <- runDB $ selectList [TeamLeagueId ==. leagueId] [Asc TeamNumber]
    leagues <- getLeaguesByUser maybeUserId
    Entity selectedSeasonId selectedSeason <- getSelectedSeason leagueId
    seasons <- runDB $ selectList [SeasonLeagueId ==. leagueId] [Desc SeasonId]
    let settingsRoute = case maybeTeam of
            Just (Entity _ team) -> LeagueTeamSettingsR leagueId $ teamNumber team
            Nothing -> LeagueSettingsR leagueId LeagueEditSettingsR
    defaultLayout $ do
        setTitle $ toMarkup $ leagueLayoutTitle league activeTab
        $(widgetFile "layouts/league")
        where activeClass :: Text -> Text 
              activeClass tabName = if tabName == activeTab then "active" else ""

leagueLayoutTitle :: League -> Text -> Text
leagueLayoutTitle league subtitle = leagueName league ++ " | " ++ subtitle

getCurrentTeam :: LeagueId -> Maybe UserId -> Handler (Maybe (Entity Team))
getCurrentTeam leagueId maybeUserId =
    runDB $ selectFirst [TeamLeagueId ==. leagueId, TeamOwnerId ==. maybeUserId, TeamOwnerId !=. Nothing] []


-------------
-- Queries --
-------------
-- TODO - maybe this query should be in a different place?
getTeamsOrderBy :: (PersistField a) =>
                   SeasonId -> Bool -> EntityField TeamSeason a ->
                   Handler [(Entity Team, Entity TeamSeason)]
getTeamsOrderBy seasonId orderAsc attribute = runDB
    $ E.select
    $ E.from $ \(team `E.InnerJoin` teamSeason) -> do
        E.on $ team ^. TeamId E.==. teamSeason ^. TeamSeasonTeamId
            E.&&. teamSeason ^. TeamSeasonSeasonId E.==. E.val seasonId
        E.orderBy [if orderAsc then E.asc  (teamSeason ^. attribute)
                               else E.desc (teamSeason ^. attribute)]
        return (team, teamSeason)


---------------------
-- League Settings --
---------------------
leagueSettingsLayout :: LeagueId -> Route App -> Enctype -> Widget -> Text -> Handler Html
leagueSettingsLayout leagueId action enctype widget activePill = do
    maybeUserId <- maybeAuthId
    league <- runDB $ get404 leagueId
    maybeTeam <- getCurrentTeam leagueId maybeUserId
    Entity _ season <- getSelectedSeason leagueId
    let disableFields = disableSettingsFields maybeUserId (map entityVal maybeTeam) league season action
    leagueLayout leagueId "Settings" $ do
        $(widgetFile "layouts/settings")

isLeagueManager :: Maybe UserId -> League -> Bool
isLeagueManager (Just userId) league = userId == leagueCreatedBy league
isLeagueManager Nothing _ = False

canEnterDraftResults :: Maybe UserId -> League -> Season -> Bool
canEnterDraftResults maybeUserId league season =
    isLeagueManager maybeUserId league &&
        (not $ seasonIsDraftComplete season) &&
        (not $ seasonIsSeasonComplete season)

isTeamOwner :: Maybe UserId -> Maybe Team -> Bool
isTeamOwner (Just userId) (Just team) = Just userId == teamOwnerId team
isTeamOwner _ _ = False

disableSettingsFields :: Maybe UserId -> Maybe Team -> League -> Season -> Route App -> Bool
disableSettingsFields maybeUserId _ league _ (LeagueSettingsR _ LeagueTeamsSettingsR) =
    not $ isLeagueManager maybeUserId league
disableSettingsFields maybeUserId _ league _ (LeagueSettingsR _ LeagueEditSettingsR) =
    not $ isLeagueManager maybeUserId league
disableSettingsFields maybeUserId _ league season (LeagueSettingsR _ _) =
    seasonIsDraftComplete season || (not $ isLeagueManager maybeUserId league)
disableSettingsFields maybeUserId maybeTeam _ _ (LeagueTeamSettingsR _ _) =
    not $ isTeamOwner maybeUserId maybeTeam
disableSettingsFields _ _ _ _ _ =
    error "Shouldn't be trying to disable fields for any other actions"

leagueOrRedirectIfIncomplete :: LeagueId -> Handler League
leagueOrRedirectIfIncomplete leagueId = do
    league <- runDB $ get404 leagueId
    if leagueIsSetupComplete league then return league else redirect $ leagueSetupNextStepToComplete league

