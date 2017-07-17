module Handler.Event where

import Import

import Handler.Score (upsertPlays, updatePlayNotes)

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
        E.where_ (event ^. EventEpisodeId E.==. E.val episodeId)
        E.orderBy [E.asc (event ^. EventTimeInEpisode)]
        return (event, episode, character, receivingCharacter)

getEvents :: Handler [FullEvent]
getEvents = runDB
    $ E.select
    $ E.from $ \(event `E.InnerJoin` episode `E.InnerJoin` character `E.LeftOuterJoin` receivingCharacter) -> do
        E.on $ E.just (event ^. EventReceivingCharacterId) E.==. E.just (receivingCharacter ?. CharacterId)
        E.on $ event ^. EventCharacterId E.==. character ^. CharacterId
        E.on $ event ^. EventEpisodeId E.==. episode ^. EpisodeId
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
            E.where_ (event ^. EventCharacterId E.==. E.val characterId E.||.
                      event ^. EventReceivingCharacterId E.==. E.just (E.val characterId))
            E.orderBy [E.asc (episode ^. EpisodeOverallNumber), E.asc (event ^. EventTimeInEpisode)]
            return (event, episode, series, actingCharacter, receivingCharacter)

---------------
-- Callbacks --
---------------
updateEventRelations :: Maybe Event -> Entity Event -> Handler ()
updateEventRelations maybeOldEvent (Entity eventId event) = do
    let userId = eventUpdatedBy event
    now <- liftIO getCurrentTime
    if eventCoreHasChanged maybeOldEvent event
        then do -- backgroundHandler $ do
            markEpisodeEventsPending (eventEpisodeId event) userId now
            updateCharacterAppearances maybeOldEvent event userId now
            updateCharacterStatus maybeOldEvent event userId now
            upsertPlays $ Entity eventId event
        else if map eventNote maybeOldEvent == Just (eventNote event) then return () else
            updatePlayNotes $ Entity eventId event

markEpisodeEventsPending :: EpisodeId -> UserId -> UTCTime -> Handler ()
markEpisodeEventsPending episodeId userId now = runDB $ do
    episode <- get404 episodeId
    if episodeStatus episode == EventsPending then return () else
        update episodeId [ EpisodeAreEventsComplete =. False
                         , EpisodeStatus =. EventsPending
                         , EpisodeEventsPendingAt =. Just (fromMaybe now $ episodeEventsPendingAt episode)
                         , EpisodeUpdatedBy =. userId
                         , EpisodeUpdatedAt =. now ]

updateCharacterAppearances :: Maybe Event -> Event -> UserId -> UTCTime -> Handler ()
updateCharacterAppearances maybeOldEvent event userId now = do
    for_ maybeOldEvent $ changeCharacterAppearances (-1) userId now
    changeCharacterAppearances 1 userId now event

updateCharacterStatus :: Maybe Event -> Event -> UserId -> UTCTime -> Handler ()
updateCharacterStatus maybeOldEvent event userId now = do
    for_ maybeOldEvent $ updateCharacterStatusForEvent True userId now
    updateCharacterStatusForEvent False userId now event

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
eventCoreHasChanged :: Maybe Event -> Event -> Bool
eventCoreHasChanged Nothing _ = True
eventCoreHasChanged (Just oldEvent) event =
    eventCharacterId oldEvent /= eventCharacterId event ||
    eventAction oldEvent /= eventAction event ||
    eventReceivingCharacterId oldEvent /= eventReceivingCharacterId event

