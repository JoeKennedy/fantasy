module Handler.Admin.Score where

import Import

import Handler.Admin
import Handler.Admin.Episode (eventTimeInEpisodeText)
import Handler.Admin.Record
import Handler.Episode       (getEpisodes)
import Handler.Event         (getEpisodeEvents)

import Data.UUID (UUID)

-----------
-- Forms --
-----------
scoreEpisodeForm :: UserId -> EpisodeId -> [Entity Character] -> [Entity Event] -> Form [Event]
scoreEpisodeForm userId episodeId characters events extra = do
    let characterOptions = characterKeys characters
    forms <- forM events $ scoreEpisodeEventForm $ characterOptions
    now <- liftIO getCurrentTime

    let eventsResult = for forms (\(Entity _ e, cField, aField, rcField, nField, tField) -> Event
            <$> fst cField
            <*> fst aField
            <*> fst rcField
            <*> pure episodeId
            <*> fst nField
            <*> fst tField
            <*> pure (eventMarkedForDestruction e)
            <*> pure (eventUuid e)
            <*> pure (eventCreatedBy e)
            <*> pure (eventCreatedAt e)
            <*> updatedByField userId
            <*> pure now)

    return (eventsResult, $(widgetFile "admin/score_episode_form"))

    where scoreEpisodeEventForm characterOptions (Entity eId e) = do
              let rcClass = if isMultiCharacter (eventAction e) then "" else " hidden"
                  rcInput = inputSmClass $ "receiving-character" ++ rcClass
              cField  <- mreq (selectField characterOptions) (inputSmClass "character")
                              $ Just $ eventCharacterId e
              aField  <- mreq (selectField optionsEnum) (inputSmClass "action")
                              $ Just $ eventAction e
              rcField <- mopt (selectField characterOptions)  rcInput
                              $ Just $ eventReceivingCharacterId e
              nField  <- mopt textField (inputSmClass "note") $ Just $ eventNote e
              tField  <- mreq timeInEpisodeField (inputSmClass "time")
                              $ Just $ eventTimeInEpisode e
              return (Entity eId e, cField, aField, rcField, nField, tField)

scoreEventForm :: UserId -> EpisodeId -> UTCTime -> UUID -> Maybe Event ->
                  FormInput Handler Event
scoreEventForm userId episodeId now uuid event = Event
    <$> ireq (selectField characters) "character"
    <*> ireq (selectField optionsEnum) "action"
    <*> iopt (selectField characters) "receivingCharacter"
    <*> pure episodeId
    <*> iopt textField "note"
    <*> ireq timeInEpisodeField "time"
    <*> existingElseDefault False (eventMarkedForDestruction <$> event)
    <*> pure uuid
    <*> createdByField userId (eventCreatedBy <$> event)
    <*> existingElseDefault now (eventCreatedAt <$> event)
    <*> updatedByField userId
    <*> pure now
    where characters = optionsPersistKey [] [Asc CharacterName] characterName

characterKeys :: [Entity Character] -> Handler (OptionList CharacterId)
characterKeys characters = fmap mkOptionList $ do
    mr <- getMessageRender
    return $ map (\(Entity characterId character) -> Option
        { optionDisplay = mr (characterName character)
        , optionInternalValue = characterId
        , optionExternalValue = toPathPiece characterId
        }) characters


------------
-- Routes --
------------
getAdminScoreR :: Handler Html
getAdminScoreR = do
    episodesAndSeries <- getEpisodes
    let seriesList = groupByFirst episodesAndSeries
    adminLayout "Score" "Score" $(widgetFile "admin/score")

getAdminScoreEpisodeR :: EpisodeId -> Handler Html
getAdminScoreEpisodeR episodeId = do
    episode <- runDB $ get404 episodeId
    if episodeTimesFinalized episode /= 0 || episodeAreEventsComplete episode
        then do
            events <- getEpisodeEvents episodeId
            adminLayout "Score" "Score" $(widgetFile "admin/scored_episode")

        else do
            userId <- requireAuthId
            events <- runDB $ selectList [ EventEpisodeId ==. episodeId
                                         , EventMarkedForDestruction ==. False
                                         ] [Asc EventTimeInEpisode]
            characters <- runDB $ selectList [] [Asc CharacterName]
            (widget, enctype) <- generateFormPost $ scoreEpisodeForm userId episodeId characters events
            adminLayout "Score" "Score" $(widgetFile "admin/score_episode")

postAdminScoreEventR :: EpisodeId -> UUID -> Handler Value
postAdminScoreEventR episodeId uuid = do
    maybeEventEntity <- runDB $ getBy $ UniqueEventEpisodeIdUuid episodeId uuid
    let maybeEventId = map entityKey maybeEventEntity
        maybeEvent   = map entityVal maybeEventEntity
    userId <- requireAuthId
    now <- liftIO getCurrentTime
    result <- runInputPostResult $ scoreEventForm userId episodeId now uuid maybeEvent
    case result of
        FormMissing -> badMethod
        FormFailure args -> invalidArgs args
        FormSuccess event -> do
            uniqueFailureOrEventId <- runDB $ replacertUnique maybeEventId event
            case uniqueFailureOrEventId of
                Right eventId -> do
                    performCallback maybeEvent $ Entity eventId event
                    returnJson $ toPathPiece eventId
                Left uniqueFailure ->
                    invalidArgs $ map (unHaskellName . fst) $ persistUniqueToFieldNames uniqueFailure

