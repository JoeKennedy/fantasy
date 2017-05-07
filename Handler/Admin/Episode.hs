module Handler.Admin.Episode where

import Import

import Handler.Admin
import Handler.Episode
import Handler.Event

import Text.Read (readMaybe)

-----------
-- Forms --
-----------
episodeForm :: UserId -> Maybe Episode -> Form Episode
episodeForm userId episode = renderBootstrapForm $ Episode
    <$> areq textField (fieldName "Name") (episodeName <$> episode)
    <*> areq intField  (fieldName "Number in season") (episodeNumber <$> episode)
    <*> areq intField  (fieldName "Number overall") (episodeOverallNumber <$> episode)
    <*> areq utcDateField (fieldName "Original air time (UTC)") (episodeAirTime <$> episode)
    <*> areq (selectField seriesOptions) (fieldName "Season") (episodeSeriesId <$> episode)
    <*> existingElseDefault YetToAir (episodeStatus <$> episode)
    <*> episodeAreEventsCompleteField episode
    <*> existingElseDefault 0 (episodeTimesFinalized <$> episode)
    <*> createdByField userId (episodeCreatedBy <$> episode)
    <*> createdAtField (episodeCreatedAt <$> episode)
    <*> updatedByField userId
    <*> updatedAtField
    where seriesOptions = optionsPersistKey [] [Desc SeriesNumber] seriesNumberText


------------
-- Fields --
------------
utcDateField :: Field Handler UTCTime
utcDateField = Field
    { fieldParse  = parseHelper $ parseUTCDate . unpack
    , fieldView   = \theId name attrs val isReq ->
        [whamlet|
          <input id=#{theId} name=#{name} *{attrs} type="text" step=any :isReq:required="" value="#{showVal val}" placeholder="yyyy-mm-dd hh:mm:ss" maxlength="#{datetimeLength}">
        |]
    , fieldEnctype = UrlEncoded
    }
    where datetimeLength = 19 :: Int
          showVal = either id (pack . take datetimeLength . show)

parseUTCDate :: String -> Either FormMessage UTCTime
parseUTCDate = maybe (Left MsgInvalidDay) Right . readMaybe

episodeAreEventsCompleteField :: (MonadHandler m,
                                  RenderMessage (HandlerSite m) FormMessage) =>
                                 Maybe Episode -> AForm m Bool 
episodeAreEventsCompleteField Nothing = pure False
episodeAreEventsCompleteField (Just episode) = do
    -- TODO - force a minimum of at least X events for the episode, and figure
    -- out what X should be (if not 0)
    if episodeStatus episode == Aired && not (episodeAreEventsComplete episode)
        then areq checkBoxField (fieldName "Is Episode Finalized?") $ Just False
        else pure $ episodeAreEventsComplete episode

eventTimeInEpisodeText :: Event -> Text
eventTimeInEpisodeText = pack . displayTime . eventTimeInEpisode


------------
-- Routes --
------------
getAdminEpisodesR :: Handler Html
getAdminEpisodesR = adminEpisodesR

postAdminEpisodesR :: Handler Html
postAdminEpisodesR = adminEpisodesR

getAdminEpisodeR :: EpisodeId -> Handler Html
getAdminEpisodeR = adminEpisodeR

postAdminEpisodeR :: EpisodeId -> Handler Html
postAdminEpisodeR = adminEpisodeR

deleteAdminEpisodeR :: EpisodeId -> Handler ()
deleteAdminEpisodeR = adminDelete "Episode"


-------------
-- Helpers --
-------------
adminEpisodesR :: Handler Html
adminEpisodesR = do
    userId   <- requireAuthId
    episodes <- getEpisodes
    let form = episodeForm userId Nothing
    adminList "episode" form $(widgetFile "admin/entities/episodes")

adminEpisodeR :: EpisodeId -> Handler Html
adminEpisodeR episodeId = do
    userId  <- requireAuthId
    episode <- runDB $ get404 episodeId
    events  <- getEpisodeEvents episodeId
    let form = episodeForm userId $ Just episode
        eventsTable = Just ("event", $(widgetFile "admin/entities/events"))
    adminShow "episode" form (Entity episodeId episode) eventsTable

