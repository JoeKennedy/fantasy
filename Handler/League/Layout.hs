module Handler.League.Layout where

import Import

import Handler.Common       (extractValueMaybe)
import Handler.League.Setup

import qualified Database.Esqueleto as E
import           Database.Esqueleto ((^.))
import           Text.Blaze         (toMarkup)

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

getLeaguesByUser :: (YesodPersist site, YesodPersistBackend site ~ SqlBackend) =>
              Maybe UserId -> HandlerT site IO [Entity League]
getLeaguesByUser maybeUserId = runDB
    $ E.select
    $ E.from $ \(team `E.InnerJoin` league) -> do
        E.on $ team ^. TeamLeagueId E.==. league ^. LeagueId
        E.where_ (team ^. TeamOwnerId E.==. E.val maybeUserId)
        E.orderBy [E.asc (league ^. LeagueName)]
        return league

getCurrentTeam :: (YesodPersist site, YesodPersistBackend site ~ SqlBackend) =>
                  LeagueId -> Maybe UserId -> HandlerT site IO (Maybe (Entity Team))
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

isTeamOwner :: Maybe UserId -> Maybe Team -> Bool
isTeamOwner (Just userId) (Just team) = Just userId == teamOwnerId team
isTeamOwner _ _ = False

disableSettingsFields :: Maybe UserId -> Maybe Team -> League -> Route App -> Bool
disableSettingsFields maybeUserId _ league (LeagueSettingsR _ _) = not $ isLeagueManager maybeUserId league
disableSettingsFields maybeUserId maybeTeam _ (LeagueTeamSettingsR _ _) = not $ isTeamOwner maybeUserId maybeTeam
disableSettingsFields _ _ _ _ = error "Shouldn't be trying to disable fields for any other actions"

leagueOrRedirectIfIncomplete :: (YesodPersist site, RedirectUrl site (Route App),
                                 YesodPersistBackend site ~ SqlBackend) =>
                                LeagueId -> HandlerT site IO League
leagueOrRedirectIfIncomplete leagueId = do
    league <- runDB $ get404 leagueId
    if leagueIsSetupComplete league then return league else redirect $ leagueSetupNextStepToComplete league

