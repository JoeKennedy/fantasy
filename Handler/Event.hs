module Handler.Event where

import Import

import Handler.Score (deleteEvent, upsertPlays, updatePlayNotes)

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
deleteEventRelations :: UserId -> Entity Event -> Handler Bool
deleteEventRelations userId (Entity eventId _) = do
    now <- liftIO getCurrentTime
    runDB $ update eventId [ EventMarkedForDestruction =. True
                           , EventUpdatedBy =. userId
                           , EventUpdatedAt =. now ]
    backgroundHandler $ deleteEvent eventId
    return False

changeEventRelations :: Maybe Event -> Entity Event -> Handler ()
changeEventRelations maybeOldEvent (Entity eventId event) =
    if eventCoreHasChanged maybeOldEvent event
        then do -- backgroundHandler $ do
            let (episodeId, userId) = (eventEpisodeId event, eventUpdatedBy event)
            episode <- runDB $ get404 episodeId
            markEpisodeEventsPending (Entity episodeId episode) userId
            updateCharacterAppearances maybeOldEvent event userId
            updateCharacterStatus maybeOldEvent event userId
            upsertPlays episode $ Entity eventId event
        else if map eventNote maybeOldEvent == Just (eventNote event) then return () else
            updatePlayNotes eventId event

markEpisodeEventsPending :: Entity Episode -> UserId -> Handler ()
markEpisodeEventsPending (Entity episodeId episode) userId = runDB $ do
    now <- liftIO getCurrentTime
    if episodeStatus episode == EventsPending then return () else
        update episodeId [ EpisodeAreEventsComplete =. False
                         , EpisodeStatus =. EventsPending
                         , EpisodeEventsPendingAt =. Just (fromMaybe now $ episodeEventsPendingAt episode)
                         , EpisodeUpdatedBy =. userId
                         , EpisodeUpdatedAt =. now ]

updateCharacterAppearances :: Maybe Event -> Event -> UserId -> Handler ()
updateCharacterAppearances maybeOldEvent event userId = do
    for_ maybeOldEvent $ changeCharacterAppearances (-1) userId
    changeCharacterAppearances 1 userId event

updateCharacterStatus :: Maybe Event -> Event -> UserId -> Handler ()
updateCharacterStatus maybeOldEvent event userId = do
    for_ maybeOldEvent $ updateCharacterStatusForEvent True userId
    updateCharacterStatusForEvent False userId event

changeCharacterAppearances :: Int -> UserId -> Event -> Handler ()
changeCharacterAppearances number userId event =
    if eventAction event /= Appear then return () else
        incrementCharacterAppearances (eventCharacterId event) number userId

incrementCharacterAppearances :: CharacterId -> Int -> UserId -> Handler ()
incrementCharacterAppearances characterId number userId = runDB $ do
    now <- liftIO getCurrentTime
    update characterId [ CharacterEpisodesAppearedIn +=. number
                       , CharacterUpdatedBy =. userId
                       , CharacterUpdatedAt =. now ]

updateCharacterStatusForEvent :: Bool -> UserId -> Event -> Handler ()
updateCharacterStatusForEvent reverseStatus userId event = do
    let (charId, mRecCharId) = (eventCharacterId event, eventReceivingCharacterId event)
    character <- runDB $ get404 charId
    mRecCharacter <- runDB $ mapM get404 mRecCharId
    let recCharPlayable = fromMaybe False $ map characterIsPlayable mRecCharacter
        killDeathStatus = if reverseStatus then Alive else Dead
        raiseStatus     = if reverseStatus then Dead  else Alive

    if not recCharPlayable then return () else
        case (eventAction event, characterIsPlayable character, mRecCharId) of
            (Kill,  _, Just rCId) -> changeCharacterStatus rCId killDeathStatus userId
            (Raise, _, Just rCId) -> changeCharacterStatus rCId raiseStatus userId
            (Death, True, _)      -> changeCharacterStatus charId killDeathStatus userId
            (_, _, _) -> return ()

changeCharacterStatus :: CharacterId -> CharacterStatus -> UserId -> Handler ()
changeCharacterStatus characterId status userId = runDB $ do
    now <- liftIO getCurrentTime
    update characterId [ CharacterStatus    =. status
                       , CharacterUpdatedBy =. userId
                       , CharacterUpdatedAt =. now ]

-------------
-- Helpers --
-------------
eventCoreHasChanged :: Maybe Event -> Event -> Bool
eventCoreHasChanged Nothing _ = True
eventCoreHasChanged (Just oldEvent) event =
    eventCharacterId oldEvent /= eventCharacterId event ||
    eventAction oldEvent /= eventAction event ||
    eventReceivingCharacterId oldEvent /= eventReceivingCharacterId event

