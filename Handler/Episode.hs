module Handler.Episode where

import Import

import Handler.Event         (getEpisodeEvents, incrementCharacterAppearances)
import Handler.League.Season (createWeekData_)
import Handler.Score         (finalizeWeek, upsertPlays)

import qualified Database.Esqueleto as E
import           Database.Esqueleto ((^.))
import           Text.Blaze (toMarkup)

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
finalizeEpisode :: EpisodeId -> Handler ()
finalizeEpisode episodeId = do
    userId <- requireAuthId
    now <- liftIO getCurrentTime
    backgroundHandler $ do
        incrementEpisodeTimesFinalized episodeId userId now
        upsertEpisodeAppearanceEvents episodeId userId now
        leagueIds <- runDB $ selectKeysList [LeagueIsActive ==. True] [Asc LeagueId]
        forM_ leagueIds $ finalizeWeek episodeId userId

incrementEpisodeTimesFinalized :: EpisodeId -> UserId -> UTCTime -> Handler ()
incrementEpisodeTimesFinalized episodeId userId now =
    runDB $ update episodeId [ EpisodeTimesFinalized +=. 1
                             , EpisodeUpdatedBy =. userId
                             , EpisodeUpdatedAt =. now ]

upsertEpisodeAppearanceEvents :: EpisodeId -> UserId -> UTCTime -> Handler ()
upsertEpisodeAppearanceEvents episodeId userId now = do
    events <- runDB $ selectList [EventEpisodeId ==. episodeId] [Asc EventTimeInEpisode]
    forM_ events $ upsertAppearanceEvents userId now

upsertAppearanceEvents :: UserId -> UTCTime -> Entity Event -> Handler ()
upsertAppearanceEvents userId now (Entity _ event) = do
    upsertAppearanceEvent userId now event $ eventCharacterId event
    mapM_ (upsertAppearanceEvent userId now event) $ eventReceivingCharacterId event

upsertAppearanceEvent :: UserId -> UTCTime -> Event -> CharacterId -> Handler ()
upsertAppearanceEvent userId now event characterId = do
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
            let event' = Event { eventCharacterId = characterId
                               , eventAction = Appear
                               , eventReceivingCharacterId = Nothing
                               , eventEpisodeId = eventEpisodeId event
                               , eventNote = Nothing
                               , eventTimeInEpisode = eventTimeInEpisode event
                               , eventCreatedBy = userId
                               , eventCreatedAt = now
                               , eventUpdatedBy = userId
                               , eventUpdatedAt = now
                               }
            eventId <- runDB $ insert event'
            incrementCharacterAppearances characterId 1 userId now
            upsertPlays $ Entity eventId event'


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
                seasons <- runDB $ selectList [SeasonIsActive ==. True] [Asc SeasonId]
                mapM_ (createWeekData_ $ Entity episodeId episode) seasons


-------------
-- Helpers --
-------------
episodeToText :: Episode -> Text
episodeToText episode = (pack . show . episodeOverallNumber) episode ++ ": " ++ episodeName episode

