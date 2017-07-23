module Handler.Event where

import Import

import Handler.Score (deletePlays, upsertPlays, updatePlayNotes)

import qualified Database.Esqueleto as E
import           Database.Esqueleto ((^.), (?.))

-----------
-- Types --
-----------
type FullEvent = (Entity Event, Entity Episode, Entity Character, Maybe (Entity Character))
type FullEventSeries = (Entity Event, Entity Episode, Entity Series, Entity Character, Maybe (Entity Character))


-------------
-- Queries --
-------------
getEpisodeEvents :: EpisodeId -> Handler [FullEvent]
getEpisodeEvents episodeId = runDB
    $ E.select
    $ E.from $ \(event `E.InnerJoin` episode `E.InnerJoin` character `E.LeftOuterJoin` receivingCharacter) -> do
        E.on $ E.just (event ^. EventReceivingCharacterId) E.==. E.just (receivingCharacter ?. CharacterId)
        E.on $ event ^. EventCharacterId E.==. character ^. CharacterId
        E.on $ event ^. EventEpisodeId E.==. episode ^. EpisodeId
        E.where_ $ event ^. EventEpisodeId E.==. E.val episodeId
            E.&&.  event ^. EventMarkedForDestruction E.==. E.val False
        E.orderBy [E.asc (event ^. EventTimeInEpisode)]
        return (event, episode, character, receivingCharacter)

getEvents :: Handler [FullEvent]
getEvents = runDB
    $ E.select
    $ E.from $ \(event `E.InnerJoin` episode `E.InnerJoin` character `E.LeftOuterJoin` receivingCharacter) -> do
        E.on $ E.just (event ^. EventReceivingCharacterId) E.==. E.just (receivingCharacter ?. CharacterId)
        E.on $ event ^. EventCharacterId E.==. character ^. CharacterId
        E.on $ event ^. EventEpisodeId E.==. episode ^. EpisodeId
        E.where_ $ event ^. EventMarkedForDestruction E.==. E.val False
        E.orderBy [E.asc (episode ^. EpisodeOverallNumber), E.asc (event ^. EventTimeInEpisode)]
        return (event, episode, character, receivingCharacter)

getCharacterEvents :: CharacterId -> Handler [FullEventSeries]
getCharacterEvents characterId = runDB
    $ E.select
    $ E.from $ \(event `E.InnerJoin` episode `E.InnerJoin` series `E.InnerJoin` actingCharacter `E.LeftOuterJoin` receivingCharacter) -> do
        E.on $ E.just (event ^. EventReceivingCharacterId) E.==. E.just (receivingCharacter ?. CharacterId)
        E.on $ event ^. EventCharacterId E.==. actingCharacter ^. CharacterId
        E.on $ episode ^. EpisodeSeriesId E.==. series ^. SeriesId
        E.on $ event ^. EventEpisodeId E.==. episode ^. EpisodeId
        E.where_ $ event ^. EventMarkedForDestruction E.==. E.val False
            E.&&. (event ^. EventCharacterId E.==. E.val characterId
            E.||.  event ^. EventReceivingCharacterId E.==. E.just (E.val characterId))
        E.orderBy [E.asc (episode ^. EpisodeOverallNumber), E.asc (event ^. EventTimeInEpisode)]
        return (event, episode, series, actingCharacter, receivingCharacter)


---------------
-- Callbacks --
---------------
createEventRelations :: Entity Event -> Handler ()
createEventRelations (Entity eventId event) =
    changeEventRelations eventId Nothing (Just event) $ eventCreatedBy event

updateEventRelations :: Event -> Entity Event -> Handler ()
updateEventRelations oldEvent (Entity eventId event) =
    changeEventRelations eventId (Just oldEvent) (Just event) $ eventUpdatedBy event

deleteEventRelations :: UserId -> Entity Event -> Handler ()
deleteEventRelations userId (Entity eventId event) =
    changeEventRelations eventId (Just event) Nothing userId

changeEventRelations :: EventId -> Maybe Event -> Maybe Event -> UserId -> Handler ()
changeEventRelations eventId maybeOldEvent maybeNewEvent userId = do
    now <- liftIO getCurrentTime
    if eventCoreHasChanged maybeOldEvent maybeNewEvent
        then do -- backgroundHandler $ do
            let episodeId = episodeIdFromEventChanges maybeOldEvent maybeNewEvent
            episode <- runDB $ get404 episodeId
            markEpisodeEventsPending (Entity episodeId episode) userId now
            updateCharacterAppearances maybeOldEvent maybeNewEvent userId now
            updateCharacterStatus maybeOldEvent maybeNewEvent userId now
            case maybeNewEvent of
                Just newEvent -> upsertPlays episode $ Entity eventId newEvent
                Nothing -> deletePlays eventId
        else if map eventNote maybeOldEvent == map eventNote maybeNewEvent then return () else
            mapM_ (updatePlayNotes eventId) maybeNewEvent

episodeIdFromEventChanges :: Maybe Event -> Maybe Event -> EpisodeId
episodeIdFromEventChanges _ (Just newEvent) = eventEpisodeId newEvent
episodeIdFromEventChanges (Just oldEvent) _ = eventEpisodeId oldEvent
episodeIdFromEventChanges _ _ = error "Must have an episode ID!"

markEpisodeEventsPending :: Entity Episode -> UserId -> UTCTime -> Handler ()
markEpisodeEventsPending (Entity episodeId episode) userId now = runDB $ do
    if episodeStatus episode == EventsPending then return () else
        update episodeId [ EpisodeAreEventsComplete =. False
                         , EpisodeStatus =. EventsPending
                         , EpisodeEventsPendingAt =. Just (fromMaybe now $ episodeEventsPendingAt episode)
                         , EpisodeUpdatedBy =. userId
                         , EpisodeUpdatedAt =. now ]

updateCharacterAppearances :: Maybe Event -> Maybe Event -> UserId -> UTCTime -> Handler ()
updateCharacterAppearances maybeOldEvent maybeNewEvent userId now = do
    for_ maybeOldEvent $ changeCharacterAppearances (-1) userId now
    for_ maybeNewEvent $ changeCharacterAppearances   1  userId now

updateCharacterStatus :: Maybe Event -> Maybe Event -> UserId -> UTCTime -> Handler ()
updateCharacterStatus maybeOldEvent maybeNewEvent userId now = do
    for_ maybeOldEvent $ updateCharacterStatusForEvent True  userId now
    for_ maybeNewEvent $ updateCharacterStatusForEvent False userId now

changeCharacterAppearances :: Int -> UserId -> UTCTime -> Event -> Handler ()
changeCharacterAppearances number userId now event =
    if eventAction event /= Appear then return () else
        incrementCharacterAppearances (eventCharacterId event) number userId now

incrementCharacterAppearances :: CharacterId -> Int -> UserId -> UTCTime -> Handler ()
incrementCharacterAppearances characterId number userId now = runDB $
        update characterId [ CharacterEpisodesAppearedIn +=. number
                           , CharacterUpdatedBy =. userId
                           , CharacterUpdatedAt =. now ]

updateCharacterStatusForEvent :: Bool -> UserId -> UTCTime -> Event -> Handler ()
updateCharacterStatusForEvent reverseStatus userId now event = do
    let (charId, mRecCharId) = (eventCharacterId event, eventReceivingCharacterId event)
    character <- runDB $ get404 charId
    mRecCharacter <- runDB $ mapM get404 mRecCharId
    let recCharPlayable = fromMaybe False $ map characterIsPlayable mRecCharacter
        killDeathStatus = if reverseStatus then Alive else Dead
        raiseStatus     = if reverseStatus then Dead  else Alive

    if not recCharPlayable then return () else
        case (eventAction event, characterIsPlayable character, mRecCharId) of
            (Kill,  _, Just rCId) -> changeCharacterStatus rCId killDeathStatus userId now
            (Raise, _, Just rCId) -> changeCharacterStatus rCId raiseStatus userId now
            (Death, True, _)      -> changeCharacterStatus charId killDeathStatus userId now
            (_, _, _) -> return ()

changeCharacterStatus :: CharacterId -> CharacterStatus -> UserId -> UTCTime -> Handler ()
changeCharacterStatus characterId status userId now = runDB $
    update characterId [ CharacterStatus    =. status
                       , CharacterUpdatedBy =. userId
                       , CharacterUpdatedAt =. now ]

-------------
-- Helpers --
-------------
eventCoreHasChanged :: Maybe Event -> Maybe Event -> Bool
eventCoreHasChanged Nothing Nothing = False
eventCoreHasChanged Nothing _ = True
eventCoreHasChanged _ Nothing = True
eventCoreHasChanged (Just oldEvent) (Just event) =
    eventCharacterId oldEvent /= eventCharacterId event ||
    eventAction oldEvent /= eventAction event ||
    eventReceivingCharacterId oldEvent /= eventReceivingCharacterId event

