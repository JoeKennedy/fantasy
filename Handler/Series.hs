module Handler.Series where

import Import
import Handler.Common (isAdmin, embeddedForm, groupByFirst)
import Handler.League         ( calculateScores, completeSeason
                              , determineIfTradeDeadlineHasPassed
                              , moveLeaguesToPostSeason)
import Handler.League.Week    (createWeekData_)

import qualified Database.Esqueleto as E
import           Database.Esqueleto ((^.), (?.))
import           Text.Blaze (toMarkup)
import           Text.Read (readMaybe)
import           Yesod.Form.Bootstrap3 (renderBootstrap3)

-----------
-- Forms --
-----------
seriesForm :: Maybe Series -> Form Series
seriesForm series = renderBootstrap3 defaultBootstrapForm $ Series
    <$> areq intField (fieldName "Number") (seriesNumber <$> series)

episodeForm :: SeriesId -> Maybe Episode -> Form Episode
episodeForm seriesId episode = renderBootstrap3 defaultBootstrapForm $ Episode
    <$> areq textField (fieldName "Name") (episodeName <$> episode)
    <*> areq intField  (fieldName "Number in season") (episodeNumber <$> episode)
    <*> areq intField  (fieldName "Number overall") (episodeOverallNumber <$> episode)
    <*> areq utcDateField (fieldName "Original air time (UTC)") (episodeAirTime <$> episode)
    <*> pure seriesId
    <*> existingElseDefault YetToAir (episodeStatus <$> episode)
    <*> existingElseDefault False (episodeAreEventsComplete <$> episode)
    <*> existingElseDefault 0 (episodeTimesFinalized <$> episode)

eventForm :: EpisodeId -> Maybe Event -> Form Event
eventForm episodeId event = renderBootstrap3 defaultBootstrapForm $ Event
    <$> areq (selectField characters)  (fieldName "Character") (eventCharacterId <$> event)
    <*> areq (selectField optionsEnum) (fieldName "Action") (eventAction <$> event)
    <*> aopt (selectField characters)  (fieldName "Receiving Character") (eventReceivingCharacterId <$> event)
    <*> pure episodeId
    <*> aopt textField (fieldName "Further description") (eventNote <$> event)
    <*> areq intField  (fieldName "Time in episode") (eventTimeInEpisode <$> event)
    where characters = optionsPersistKey [] [Asc CharacterName] characterName

------------
-- Routes --
------------
getSeriesListR :: Handler Html
getSeriesListR = do
    maybeUser <- maybeAuth
    (widget, enctype) <- generateFormPost $ seriesForm Nothing
    episodesAndSeries <- runDB
        $ E.select
        $ E.from $ \(series `E.InnerJoin` episode) -> do
            E.on $ series ^. SeriesId E.==. episode ^. EpisodeSeriesId
            E.orderBy [E.asc (series ^. SeriesNumber), E.asc (episode ^. EpisodeNumber)]
            return (series, episode)
    let seriesList = groupByFirst episodesAndSeries
    defaultLayout $ do
        setTitle "Game Of Thrones Seasons"
        let action = SeriesListR
        $(widgetFile "series_list")

postSeriesListR :: Handler Html
postSeriesListR = do
    ((result, widget), enctype) <- runFormPost $ seriesForm Nothing
    case result of
        FormSuccess series -> do
            _ <- runDB $ insert series
            redirect SeriesListR
        _ -> defaultLayout $ do
            setTitle "Series creation failed"
            let action = SeriesListR
            $(widgetFile "embedded_form")

getSeriesR :: Int -> Handler Html
getSeriesR seriesNo = do
    maybeUser <- maybeAuth
    Entity seriesId series <- runDB $ getBy404 $ UniqueSeriesNumber seriesNo
    episodes <- runDB $ selectList [EpisodeSeriesId ==. seriesId] [Asc EpisodeNumber]
    (seriesWidget,  seriesEnctype)  <- generateFormPost $ seriesForm $ Just series
    (episodeWidget, episodeEnctype) <- generateFormPost $ episodeForm seriesId Nothing
    defaultLayout $ do
        setTitle $ toMarkup $ "Game Of Thrones Season " ++ show seriesNo
        let seriesAction = SeriesR seriesNo
            episodeAction = SeriesEpisodesR seriesNo
            episodesPanelTitle = "Episodes" :: String
        $(widgetFile "series")

postSeriesR :: Int -> Handler Html
postSeriesR seriesNo = do
    Entity seriesId series <- runDB $ getBy404 $ UniqueSeriesNumber seriesNo
    ((result, widget), enctype) <- runFormPost $ seriesForm $ Just series
    case result of
        FormSuccess series' -> do
            runDB $ replace seriesId series'
            redirect $ SeriesR $ seriesNumber series'
        _ -> defaultLayout $ do
            setTitle "Edit of season failed"
            let action = SeriesR seriesNo
            $(widgetFile "embedded_form")

getSeriesEpisodesR :: Int -> Handler Html
getSeriesEpisodesR seriesNo = redirect $ SeriesR seriesNo

postSeriesEpisodesR :: Int -> Handler Html
postSeriesEpisodesR seriesNo = do
    Entity seriesId series <- runDB $ getBy404 $ UniqueSeriesNumber seriesNo
    ((result, widget), enctype) <- runFormPost $ episodeForm seriesId Nothing
    case result of
        FormSuccess episode -> do
            _ <- runDB $ insert episode
            redirect $ SeriesEpisodeR (seriesNumber series) (episodeNumber episode)
        _ -> defaultLayout $ do
            setTitle "Episode creation failed"
            let action = SeriesEpisodesR $ seriesNumber series
            $(widgetFile "embedded_form")

getSeriesEpisodeR :: Int -> Int -> Handler Html
getSeriesEpisodeR seriesNo episodeNo = do
    maybeUser <- maybeAuth
    Entity seriesId  _ <- runDB $ getBy404 $ UniqueSeriesNumber seriesNo
    Entity episodeId episode <- runDB $ getBy404 $ UniqueEpisodeNumberSeries episodeNo seriesId
    (episodeWidget, episodeEnctype) <- generateFormPost $ episodeForm seriesId $ Just episode
    (eventWidget, eventEnctype)     <- generateFormPost $ eventForm episodeId Nothing
    events <- runDB
        $ E.select
        $ E.from $ \(event `E.InnerJoin` character `E.LeftOuterJoin` receivingCharacter) -> do
            E.on $ E.just (event ^. EventReceivingCharacterId) E.==. E.just (receivingCharacter ?. CharacterId)
            E.on $ event ^. EventCharacterId E.==. character ^. CharacterId
            E.where_ (event ^. EventEpisodeId E.==. E.val episodeId)
            E.orderBy [E.asc (event ^. EventTimeInEpisode)]
            return (event, character, receivingCharacter)
    defaultLayout $ do
        setTitle $ toMarkup $ "Game Of Thrones Season " ++ show seriesNo ++ " Episode " ++ show episodeNo ++ ": " ++ show (episodeName episode)
        let episodeAction = SeriesEpisodeR seriesNo episodeNo
            eventHtmlAction = SeriesEpisodeEventsR seriesNo episodeNo
            areEventsCompletable = isAdmin maybeUser && seriesNo == 6
                                && (not $ episodeAreEventsComplete episode)
                                && episodeStatus episode /= YetToAir
        $(widgetFile "episode")

postSeriesEpisodeR :: Int -> Int -> Handler Html
postSeriesEpisodeR seriesNo episodeNo = do
    Entity seriesId  _ <- runDB $ getBy404 $ UniqueSeriesNumber seriesNo
    Entity episodeId episode <- runDB $ getBy404 $ UniqueEpisodeNumberSeries episodeNo seriesId
    ((result, widget), enctype) <- runFormPost $ episodeForm seriesId $ Just episode
    case result of
        FormSuccess episode' -> do
            runDB $ replace episodeId episode'
            series' <- runDB $ get404 $ episodeSeriesId episode
            redirect $ SeriesEpisodeR (seriesNumber series') (episodeNumber episode')
        _ -> defaultLayout $ do
            setTitle "Edit of episode failed"
            let action = SeriesEpisodeR seriesNo episodeNo
            $(widgetFile "embedded_form")

postSeriesEpisodeScoreR :: Int -> Int -> Handler ()
postSeriesEpisodeScoreR seriesNo episodeNo = do
    userId <- requireAuthId
    Entity seriesId  _ <- runDB $ getBy404 $ UniqueSeriesNumber seriesNo
    Entity episodeId episode <- runDB $ getBy404 $ UniqueEpisodeNumberSeries episodeNo seriesId
    if episodeAreEventsComplete episode then return () else do
        runDB $ update episodeId [EpisodeAreEventsComplete =. True]
        -- create appear events if they don't exist already
        events <- runDB $ selectList [EventEpisodeId ==. episodeId] [Asc EventTimeInEpisode]
        mapM_ upsertAppearanceEvents events
        -- for kill and die events, change character status to Dead
        -- for raise events, change character status to Alive
        mapM_ (updateCharacterStatus userId) events

        if episodeTimesFinalized episode > 0 then return () else do
            -- for appearance events, increment episodes appeared in for character
            appearEvents <- runDB $ selectList [ EventEpisodeId ==. episodeId
                                               , EventAction ==. Appear
                                               ] [Asc EventTimeInEpisode]
            mapM_ (incrementCharacterEpisodeCount userId) appearEvents
        -- calculate scores for the events
        calculateScores episodeId
        -- move relevant leagues to postseason
        moveLeaguesToPostSeason episodeId
    redirect $ SeriesEpisodeR seriesNo episodeNo

postSeriesEpisodeEventsR :: Int -> Int -> Handler Html
postSeriesEpisodeEventsR seriesNo episodeNo = do
    Entity seriesId  _ <- runDB $ getBy404 $ UniqueSeriesNumber seriesNo
    Entity episodeId _ <- runDB $ getBy404 $ UniqueEpisodeNumberSeries episodeNo seriesId
    ((result, widget), enctype) <- runFormPost $ eventForm episodeId Nothing
    case result of
        FormSuccess event -> do
            _ <- runDB $ insert event
            redirect $ SeriesEpisodeR seriesNo episodeNo
        _ -> defaultLayout $ do
            setTitle "Episode creation failed"
            let action = SeriesEpisodeEventsR seriesNo episodeNo
            $(widgetFile "embedded_form")

getSeriesEpisodeEventR :: Int -> Int -> EventId -> Handler Html
getSeriesEpisodeEventR seriesNo episodeNo eventId = do
    Entity seriesId  _ <- runDB $ getBy404 $ UniqueSeriesNumber seriesNo
    Entity episodeId _ <- runDB $ getBy404 $ UniqueEpisodeNumberSeries episodeNo seriesId
    event <- runDB $ get404 eventId
    (widget, enctype) <- generateFormPost $ eventForm episodeId $ Just event
    defaultLayout $ do
        setTitle "Event"
        let action = SeriesEpisodeEventR seriesNo episodeNo eventId
        $(widgetFile "event")

postSeriesEpisodeEventR :: Int -> Int -> EventId -> Handler Html
postSeriesEpisodeEventR seriesNo episodeNo eventId = do
    Entity seriesId  _ <- runDB $ getBy404 $ UniqueSeriesNumber seriesNo
    Entity episodeId _ <- runDB $ getBy404 $ UniqueEpisodeNumberSeries episodeNo seriesId
    event <- runDB $ get404 eventId
    ((result, widget), enctype) <- runFormPost $ eventForm episodeId $ Just event
    case result of
        FormSuccess event' -> do
            runDB $ replace eventId event'
            redirect $ SeriesEpisodeEventR seriesNo episodeNo eventId
        _ -> defaultLayout $ do
            setTitle "Edit of event failed"
            let action = SeriesEpisodeEventR seriesNo episodeNo eventId
            $(widgetFile "event_form")

-------------
-- Widgets --
-------------
eventFormWidget :: Route App -> Enctype -> Widget -> Widget
eventFormWidget action enctype widget = $(widgetFile "event_form")

seriesEpisodesPanel :: Series -> [Entity Episode] -> String -> Widget
seriesEpisodesPanel series episodes panelTitle = $(widgetFile "series_episodes_panel")

------------
-- Fields --
------------
utcDateField :: Field Handler UTCTime
utcDateField = Field
    { fieldParse  = parseHelper $ parseUTCDate . unpack
    , fieldView   = \theId name attrs val isReq ->
        [whamlet|
          <input id="#{theId}" name="#{name}" *{attrs} type="text" step=any :isReq:required="" value="#{showVal val}" placeholder="yyyy-mm-dd hh:mm:ss" maxlength="#{datetimeLength}">
        |]
    , fieldEnctype = UrlEncoded
    }
    where datetimeLength = 19 :: Int
          showVal = either id (pack . take datetimeLength . show)

parseUTCDate :: String -> Either FormMessage UTCTime
parseUTCDate = maybe (Left MsgInvalidDay) Right . readMaybe

----------
-- Jobs --
----------
finishAiringEpisode :: Handler ()
finishAiringEpisode = do
    maybeEpisode <- runDB $ selectFirst [EpisodeStatus ==. Airing] [Asc EpisodeId]
    case maybeEpisode of
        Nothing -> return ()
        Just (Entity episodeId _) -> runDB $ update episodeId [EpisodeStatus =. Aired]

airEpisode :: Handler ()
airEpisode = do
    -- For now, grab the most recent episode yet to air
    -- TODO - come up with a way to do this using the air time
    maybeEpisode <- runDB $ selectFirst [EpisodeStatus ==. YetToAir] [Asc EpisodeId]
    case maybeEpisode of
        Nothing -> return ()
        Just (Entity episodeId episode) -> do
            runDB $ update episodeId [EpisodeStatus =. Airing]
            leagueIds <- runDB $ selectKeysList [LeagueIsActive ==. True] [Asc LeagueId]
            mapM_ (createWeekData_ $ Entity episodeId episode) leagueIds
            -- TODO - try to figure out a way not to hard-code the episode
            -- number to 10
            let episodeNo = episodeNumber episode
            if episodeNumber episode == 10 then mapM_ (completeSeason episodeNo) leagueIds else return ()
            mapM_ (determineIfTradeDeadlineHasPassed episodeNo) leagueIds


-------------
-- Helpers --
-------------
upsertAppearanceEvents :: Entity Event -> Handler ()
upsertAppearanceEvents (Entity _ event) = do
    upsertAppearanceEvent event $ eventCharacterId event
    mapM_ (upsertAppearanceEvent event) $ eventReceivingCharacterId event

upsertAppearanceEvent :: Event -> CharacterId -> Handler ()
upsertAppearanceEvent event characterId = do
    maybeAppear <- runDB $ selectFirst [ EventAction ==. Appear
                                       , EventCharacterId ==. characterId
                                       , EventEpisodeId ==. eventEpisodeId event
                                       ] [Asc EventTimeInEpisode]
    case maybeAppear of
        Just (Entity appearEventId appearEvent) ->
            if eventTimeInEpisode event >= eventTimeInEpisode appearEvent then return () else
                runDB $ update appearEventId [EventTimeInEpisode =. eventTimeInEpisode event]
        Nothing -> runDB $ insert_ Event { eventCharacterId = characterId
                                         , eventAction = Appear
                                         , eventReceivingCharacterId = Nothing
                                         , eventEpisodeId = eventEpisodeId event
                                         , eventNote = Nothing
                                         , eventTimeInEpisode = eventTimeInEpisode event
                                         }

updateCharacterStatus :: UserId -> Entity Event -> Handler ()
updateCharacterStatus userId (Entity _ event) = do
    now <- liftIO getCurrentTime
    character <- runDB $ get404 $ eventCharacterId event
    mRecCharacter <- runDB $ mapM get404 $ eventReceivingCharacterId event
    let characterPlayable = characterIsPlayable character
    let recCharacterPlayable = fromMaybe False $ map characterIsPlayable mRecCharacter
    runDB $ case (eventAction event, characterPlayable, eventReceivingCharacterId event, recCharacterPlayable) of
        (Kill, _, Just recCharacterId, True) ->
            update recCharacterId [ CharacterStatus    =. Dead
                                  , CharacterUpdatedBy =. userId
                                  , CharacterUpdatedAt =. now
                                  ]
        (Raise, _, Just recCharacterId, True) ->
            update recCharacterId [ CharacterStatus    =. Alive
                                  , CharacterUpdatedBy =. userId
                                  , CharacterUpdatedAt =. now
                                  ]
        (Death, True, _, _) ->
            update (eventCharacterId event) [ CharacterStatus    =. Dead
                                            , CharacterUpdatedBy =. userId
                                            , CharacterUpdatedAt =. now
                                            ]
        (_, _, _, _) -> return ()

incrementCharacterEpisodeCount :: UserId -> Entity Event -> Handler ()
incrementCharacterEpisodeCount userId (Entity _ event) =
    if eventAction event == Appear then do
        now <- liftIO getCurrentTime
        runDB $ update (eventCharacterId event) [ CharacterEpisodesAppearedIn +=. 1
                                                , CharacterUpdatedBy =. userId
                                                , CharacterUpdatedAt =. now
                                                ]
    else return ()

