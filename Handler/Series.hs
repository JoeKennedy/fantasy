module Handler.Series where

import Import
import Handler.Common (isAdmin)

import qualified Data.Map as Map
import qualified Database.Esqueleto as E
import           Database.Esqueleto ((^.), (?.))
import           Text.Blaze (toMarkup)
import           Text.Read (readMaybe)
import           Yesod.Form.Bootstrap3 (renderBootstrap3)

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

eventForm :: EpisodeId -> Maybe Event -> Form Event
eventForm episodeId event = renderBootstrap3 defaultBootstrapForm $ Event
    <$> areq (selectField characters)  (fieldName "Character") (eventCharacterId <$> event)
    <*> areq (selectField optionsEnum) (fieldName "Action") (eventAction <$> event)
    <*> aopt (selectField characters)  (fieldName "Receiving Character") (eventReceivingCharacterId <$> event)
    <*> pure episodeId
    <*> areq intField  (fieldName "Time in episode") (eventTimeInEpisode <$> event)
    where characters = optionsPersistKey [] [Asc CharacterName] characterName

groupByFirst :: Ord k => [(k, t)] -> Map k [t]
groupByFirst tuples = Map.fromListWith (++) [(x, [y]) | (x, y) <- tuples]

embeddedForm action enctype widget = $(widgetFile "embedded_form")

eventFormWidget action enctype widget = $(widgetFile "event_form")

seriesEpisodesPanel series episodes panelTitle = $(widgetFile "series_episodes_panel")

getSeriesListR :: Handler Html
getSeriesListR = do
    (widget, enctype) <- generateFormPost $ seriesForm Nothing
    episodesAndSeries <- runDB
        $ E.select
        $ E.from $ \(series `E.InnerJoin` episode) -> do
            E.on $ series ^. SeriesId E.==. episode ^. EpisodeSeriesId
            -- Needs to be reverse ordered because Ord for Entity seems
            -- to be the opposite of what you'd expect
            E.orderBy [E.desc (series ^. SeriesNumber), E.desc (episode ^. EpisodeNumber)]
            return (series, episode)
    let seriesList = Map.toList $ groupByFirst episodesAndSeries
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
    Entity { entityKey = seriesId, entityVal = series } <- runDB $ getBy404 $ UniqueSeriesNumber seriesNo
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
    Entity { entityKey = seriesId, entityVal = series } <- runDB $ getBy404 $ UniqueSeriesNumber seriesNo
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
    Entity { entityKey = seriesId, entityVal = series } <- runDB $ getBy404 $ UniqueSeriesNumber seriesNo
    ((result, widget), enctype) <- runFormPost $ episodeForm seriesId Nothing
    case result of
        FormSuccess episode -> do
            _ <- runDB $ insert episode
            redirect $ SeriesEpisodeR (seriesNumber series) (episodeNumber episode)
        _ -> defaultLayout $ do
            setTitle "Episode creation failed"
            let action = SeriesEpisodesR $ seriesNumber series
            $(widgetFile "embedded_form")

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

getSeriesEpisodeR :: Int -> Int -> Handler Html
getSeriesEpisodeR seriesNo episodeNo = do
    Entity { entityKey = seriesId,  entityVal = series }  <- runDB $ getBy404 $ UniqueSeriesNumber seriesNo
    Entity { entityKey = episodeId, entityVal = episode } <- runDB $ getBy404 $ UniqueEpisodeNumberSeries episodeNo seriesId
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
        $(widgetFile "episode")

postSeriesEpisodeR :: Int -> Int -> Handler Html
postSeriesEpisodeR seriesNo episodeNo = do
    Entity { entityKey = seriesId } <- runDB $ getBy404 $ UniqueSeriesNumber seriesNo
    Entity { entityKey = episodeId, entityVal = episode } <- runDB $ getBy404 $ UniqueEpisodeNumberSeries episodeNo seriesId
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

postSeriesEpisodeEventsR :: Int -> Int -> Handler Html
postSeriesEpisodeEventsR seriesNo episodeNo = do
    Entity { entityKey = seriesId  } <- runDB $ getBy404 $ UniqueSeriesNumber seriesNo
    Entity { entityKey = episodeId } <- runDB $ getBy404 $ UniqueEpisodeNumberSeries episodeNo seriesId
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
    Entity userid user <- requireAuth
    Entity { entityKey = seriesId  } <- runDB $ getBy404 $ UniqueSeriesNumber seriesNo
    Entity { entityKey = episodeId } <- runDB $ getBy404 $ UniqueEpisodeNumberSeries episodeNo seriesId
    event <- runDB $ get404 eventId
    (widget, enctype) <- generateFormPost $ eventForm episodeId $ Just event
    defaultLayout $ do
        setTitle "Event"
        let action = SeriesEpisodeEventR seriesNo episodeNo eventId
        $(widgetFile "event")

postSeriesEpisodeEventR :: Int -> Int -> EventId -> Handler Html
postSeriesEpisodeEventR seriesNo episodeNo eventId = do
    Entity { entityKey = seriesId  } <- runDB $ getBy404 $ UniqueSeriesNumber seriesNo
    Entity { entityKey = episodeId } <- runDB $ getBy404 $ UniqueEpisodeNumberSeries episodeNo seriesId
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

