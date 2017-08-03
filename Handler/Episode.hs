module Handler.Episode where

import Import

import Handler.Event         (getEpisodeEvents, incrementCharacterAppearances)
import Handler.League.Season (createWeekData_)
-- import Handler.Score         (finalizeWeek, upsertPlays)
-- TODO - remove the below line and uncomment above line
import Handler.Score         (finalizeWeek, unfinalizeWeek, upsertPlays)

import qualified Database.Esqueleto as E
import           Database.Esqueleto ((^.))
import           Data.UUID.V4       (nextRandom)
import           Text.Blaze         (toMarkup)

------------
-- Routes --
------------
getSeriesEpisodesR :: Int -> Handler Html
getSeriesEpisodesR seriesNo = redirect $ SeriesR seriesNo

getSeriesEpisodeR :: Int -> Int -> Handler Html
getSeriesEpisodeR seriesNo episodeNo = do
    Entity seriesId  _ <- runDB $ getBy404 $ UniqueSeriesNumber seriesNo
    Entity episodeId episode <- runDB $ getBy404 $ UniqueEpisodeNumberSeries episodeNo seriesId
    events <- getEpisodeEvents episodeId
    defaultLayout $ do
        setTitle $ toMarkup $ "Game Of Thrones Season " ++ show seriesNo ++ " Episode " ++ show episodeNo ++ ": " ++ show (episodeName episode)
        $(widgetFile "episode")


-------------
-- Queries --
-------------
getEpisodes :: Handler [(Entity Series, Entity Episode)]
getEpisodes = runDB
    $ E.select
    $ E.from $ \(series `E.InnerJoin` episode) -> do
        E.on $ series ^. SeriesId E.==. episode ^. EpisodeSeriesId
        E.orderBy [E.asc (series ^. SeriesNumber), E.asc (episode ^. EpisodeNumber)]
        return (series, episode)

getSeriesEpisodes :: SeriesId -> Handler [(Entity Series, Entity Episode)]
getSeriesEpisodes seriesId = runDB
    $ E.select
    $ E.from $ \(series `E.InnerJoin` episode) -> do
        E.on $ series ^. SeriesId E.==. episode ^. EpisodeSeriesId
        E.where_ (episode ^. EpisodeSeriesId E.==. E.val seriesId)
        E.orderBy [E.asc (series ^. SeriesNumber), E.asc (episode ^. EpisodeNumber)]
        return (series, episode)


---------------
-- Callbacks --
---------------
finalizeEpisode :: Entity Episode -> Handler ()
finalizeEpisode (Entity episodeId episode) = do
    now <- liftIO getCurrentTime
    let userId = episodeUpdatedBy episode
    backgroundHandler $ do
        incrementEpisodeTimesFinalized episodeId userId now
        -- TODO - delete the below 2 lines
        events <- runDB $ selectList [EventEpisodeId ==. episodeId] [Asc EventTimeInEpisode]
        forM_ events $ upsertPlays episode
        -- TODO - delete the above 2 lines
        upsertEpisodeAppearanceEvents episodeId userId now
        leagueIds <- runDB $ selectKeysList [LeagueIsActive ==. True] [Asc LeagueId]
        forM_ leagueIds $ finalizeWeek episodeId userId
        -- TODO - delete the below line maybe? I might keep it, could be nice to
        -- have
        setMessage $ toMarkup $ episodeName episode ++ " has been finalized"

-- TODO - delete this function
unfinalizeEpisode :: Entity Episode -> Handler ()
unfinalizeEpisode (Entity episodeId episode) =
    if episodeOverallNumber episode /= 61 then return () else backgroundHandler $ do
        leagueIds <- runDB $ selectKeysList [LeagueIsActive ==. True] [Asc LeagueId]
        forM_ leagueIds $ unfinalizeWeek episodeId
        setMessage $ toMarkup $ episodeName episode ++ " has been un-finalized"

incrementEpisodeTimesFinalized :: EpisodeId -> UserId -> UTCTime -> Handler ()
incrementEpisodeTimesFinalized episodeId userId now =
    runDB $ update episodeId [ EpisodeStatus =. EventsComplete
                             -- TODO - uncomment the below 2 lines
                             -- , EpisodeTimesFinalized +=. 1
                             -- , EpisodeEventsCompleteAt =. Just now
                             -- TODO - uncomment the above 2 lines
                             , EpisodeUpdatedBy =. userId
                             , EpisodeUpdatedAt =. now ]

upsertEpisodeAppearanceEvents :: EpisodeId -> UserId -> UTCTime -> Handler ()
upsertEpisodeAppearanceEvents episodeId userId now = do
    events <- runDB $ selectList [ EventEpisodeId ==. episodeId
                                 , EventMarkedForDestruction ==. False
                                 ] [Asc EventTimeInEpisode]
    forM_ events $ upsertAppearanceEvents userId now

upsertAppearanceEvents :: UserId -> UTCTime -> Entity Event -> Handler ()
upsertAppearanceEvents userId now (Entity _ event) = do
    episode <- runDB $ get404 $ eventEpisodeId event
    upsertAppearanceEvent userId now event episode $ eventCharacterId event
    mapM_ (upsertAppearanceEvent userId now event episode) $ eventReceivingCharacterId event

upsertAppearanceEvent :: UserId -> UTCTime -> Event -> Episode -> CharacterId -> Handler ()
upsertAppearanceEvent userId now event episode characterId = do
    maybeAppear <- runDB $ selectFirst [ EventAction ==. Appear
                                       , EventCharacterId ==. characterId
                                       , EventEpisodeId ==. eventEpisodeId event
                                       ] [Asc EventTimeInEpisode]

    case maybeAppear of
        Just (Entity appearEventId appearEvent) ->
            if eventTimeInEpisode event >= eventTimeInEpisode appearEvent then return () else
                runDB $ update appearEventId [ EventTimeInEpisode =. eventTimeInEpisode event
                                             , EventUpdatedBy =. userId
                                             , EventUpdatedAt =. now ]

        Nothing -> do
            uuid <- liftIO nextRandom
            let event' = Event { eventCharacterId = characterId
                               , eventAction = Appear
                               , eventReceivingCharacterId = Nothing
                               , eventEpisodeId = eventEpisodeId event
                               , eventNote = Nothing
                               , eventTimeInEpisode = eventTimeInEpisode event
                               , eventMarkedForDestruction = False
                               , eventUuid = uuid
                               , eventCreatedBy = userId
                               , eventCreatedAt = now
                               , eventUpdatedBy = userId
                               , eventUpdatedAt = now
                               }
            eventId <- runDB $ insert event'
            incrementCharacterAppearances characterId 1 userId
            upsertPlays episode $ Entity eventId event'


----------
-- Jobs --
----------
finishAiringEpisode :: Handler ()
finishAiringEpisode = do
    maybeEpisode <- runDB $ selectFirst [EpisodeStatus ==. Airing] [Asc EpisodeOverallNumber]
    case maybeEpisode of
        Nothing -> return ()
        Just (Entity episodeId _) -> runDB $ update episodeId [EpisodeStatus =. Aired]

airEpisode :: Handler ()
airEpisode = do
    maybeEpisode <- runDB $ selectFirst [EpisodeStatus ==. YetToAir] [Asc EpisodeOverallNumber]
    case maybeEpisode of
        Nothing -> return ()
        Just (Entity episodeId episode) -> do
            now <- liftIO getCurrentTime
            if utctDay now /= utctDay (episodeAirTime episode) then return () else do
                runDB $ update episodeId [EpisodeStatus =. Airing]
                seasons <- runDB $ selectList [ SeasonIsActive ==. True
                                              , SeasonSeriesId ==. episodeSeriesId episode
                                              ] [Asc SeasonId]
                mapM_ (createWeekData_ $ Entity episodeId episode) seasons


-------------
-- Helpers --
-------------
episodeToText :: Episode -> Text
episodeToText episode = (pack . show . episodeOverallNumber) episode ++ ": " ++ episodeName episode

