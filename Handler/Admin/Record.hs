module Handler.Admin.Record where

import Import

import Handler.Character     (createCharacterPerformances,
                              createCharacterPerformancesIfNecessary)
import Handler.Episode       (finalizeEpisode)
import Handler.Event         (updateEventRelations)
import Handler.League        (createPlayer)
import Handler.League.Season (createSeries6Seasons)
import Handler.Score

----------------
-- Data Types --
----------------
class AdminRecord record where
    performCallback :: Maybe record -> Entity record -> Handler ()
    performCallback (Just oldRecord) = afterUpdate oldRecord
    performCallback Nothing          = afterCreate

    afterCreate :: Entity record -> Handler ()
    afterCreate _ = return ()

    afterUpdate :: record -> Entity record -> Handler ()
    afterUpdate _ _ = return ()

    createdBy :: record -> UserId
    createdAt :: record -> UTCTime
    updatedBy :: record -> UserId
    updatedAt :: record -> UTCTime

    auditInfo :: record -> Handler (Text, Text, Text, Text)
    auditInfo r = do
        let (cBy, cAt, uBy, uAt) = (createdBy r, createdAt r, updatedBy r, updatedAt r)
        creator <- runDB $ get404 cBy
        updater <- runDB $ get404 uBy
        return ( userName $ Entity cBy creator, pack $ displayUTCTime cAt
               , userName $ Entity uBy updater, pack $ displayUTCTime uAt )


---------------
-- Instances --
---------------
instance AdminRecord Blurb where
    createdBy = blurbCreatedBy
    createdAt = blurbCreatedAt
    updatedBy = blurbUpdatedBy
    updatedAt = blurbUpdatedAt

instance AdminRecord Character where
    afterCreate (Entity characterId character) = do -- backgroundHandler $ do
        leagues <- runDB $ selectList [] [Asc LeagueId]
        mapM_ (\l -> runDB $ createPlayer l $ Entity characterId character) leagues
        createCharacterPerformances characterId

    afterUpdate _ (Entity characterId character) = do -- backgroundHandler $ do
        runDB $ updateWhere [PlayerCharacterId ==. characterId]
                            [PlayerIsPlayable =. characterIsPlayable character]
        createCharacterPerformancesIfNecessary characterId

    createdBy = characterCreatedBy
    createdAt = characterCreatedAt
    updatedBy = characterUpdatedBy
    updatedAt = characterUpdatedAt

instance AdminRecord Episode where
    afterUpdate oldEpisode (Entity episodeId episode) =
        if episodeAreEventsComplete episode && not (episodeAreEventsComplete oldEpisode)
            then finalizeEpisode episodeId
            else return ()

    createdBy = episodeCreatedBy
    createdAt = episodeCreatedAt
    updatedBy = episodeUpdatedBy
    updatedAt = episodeUpdatedAt

instance AdminRecord Event where
    afterCreate = updateEventRelations Nothing
    afterUpdate oldEvent = updateEventRelations $ Just oldEvent

    createdBy = eventCreatedBy
    createdAt = eventCreatedAt
    updatedBy = eventUpdatedBy
    updatedAt = eventUpdatedAt

instance AdminRecord House where
    createdBy = houseCreatedBy
    createdAt = houseCreatedAt
    updatedBy = houseUpdatedBy
    updatedAt = houseUpdatedAt

instance AdminRecord Series where
    -- Trigger the below function on the update of a series
    -- TODO - instead, on create, create all the new seasons
    afterUpdate _ _ = createSeries6Seasons
    createdBy = seriesCreatedBy
    createdAt = seriesCreatedAt
    updatedBy = seriesUpdatedBy
    updatedAt = seriesUpdatedAt

instance AdminRecord Species where
    createdBy = speciesCreatedBy
    createdAt = speciesCreatedAt
    updatedBy = speciesUpdatedBy
    updatedAt = speciesUpdatedAt

