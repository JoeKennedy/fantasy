module Handler.League.Layout where

import Import

import Handler.League.Setup

import qualified Database.Esqueleto           as E
import           Database.Esqueleto           ((^.))
import           Data.Random.List
import           Data.Random.RVar
import           Data.Random.Source.DevRandom
import           Data.Time                    (addUTCTime)
import           Text.Blaze                   (toMarkup)

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
    maybeDraftSettingsEntity <- runDB $ getBy $ UniqueDraftSettingsSeasonId selectedSeasonId
    maybeSeriesAndEpisode <- getHeaderSeriesAndEpisode
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


-------------
-- Helpers --
-------------
getHeaderSeriesAndEpisode :: Handler (Maybe (Entity Series, Entity Episode))
getHeaderSeriesAndEpisode = runDB $ do
    maybeSeriesEntity <- selectFirst [] [Desc SeriesNumber]
    case maybeSeriesEntity of
        Nothing -> return Nothing
        Just seriesEntity -> do
            now <- liftIO getCurrentTime
            let threeDaysFromNow = addUTCTime (3 * 24 * 60 * 60) now
            maybeEpisodeEntity <- selectFirst [ EpisodeAirTime <=. threeDaysFromNow
                                              , EpisodeSeriesId ==. entityKey seriesEntity
                                              ] [Desc EpisodeAirTime]
            return $ case maybeEpisodeEntity of Just episodeEntity -> Just (seriesEntity, episodeEntity)
                                                Nothing            -> Nothing

randomizeDraftOrderIfRelevant :: UserId -> DraftOrderType -> DraftSettings -> Entity Season -> Handler ()
randomizeDraftOrderIfRelevant userId draftOrderType draftSettings (Entity seasonId season)
    | draftOrderType /= draftSettingsDraftOrderType draftSettings = return ()
    | isJust (seasonDraftOrderDeterminedAt season) = return ()
    | otherwise = runDB $ do
        teamSeasonIds <- selectKeysList [TeamSeasonSeasonId ==. seasonId] [Asc TeamSeasonId]
        let numberRange = [1..length teamSeasonIds]
        draftOrder <- liftIO (runRVar (shuffle numberRange) DevRandom :: IO [Int])
        now <- liftIO getCurrentTime
        mapM_ (randomizeTeamDraftOrder userId now) $ zip teamSeasonIds draftOrder
        update seasonId [ SeasonDraftOrderDeterminedAt =. Just now
                        , SeasonUpdatedBy =. userId
                        , SeasonUpdatedAt =. now
                        ]

randomizeTeamDraftOrder :: UserId -> UTCTime -> (TeamSeasonId, Int) -> ReaderT SqlBackend Handler ()
randomizeTeamDraftOrder userId now (teamSeasonId, draftOrder) =
    update teamSeasonId [ TeamSeasonDraftOrder =. draftOrder
                        , TeamSeasonUpdatedBy  =. userId
                        , TeamSeasonUpdatedAt  =. now
                        ]

