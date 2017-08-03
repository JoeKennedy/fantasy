module Handler.Admin.Record where

import Import

import Handler.Character     (addCharacterToLeagues, updateCharacterInLeagues)
-- import Handler.Episode       (finalizeEpisode)
-- TODO - delete the below line and uncomment the above line
import Handler.Episode       (finalizeEpisode, unfinalizeEpisode)
import Handler.Event         (changeEventRelations, deleteEventRelations)
import Handler.League.Season (createLeagueSeasons, updateLeagueSeasonsIfRelevent)
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

    beforeDelete :: UserId -> Entity record -> Handler Bool
    beforeDelete _ _ = return True

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
    afterCreate = addCharacterToLeagues
    afterUpdate = updateCharacterInLeagues

    createdBy = characterCreatedBy
    createdAt = characterCreatedAt
    updatedBy = characterUpdatedBy
    updatedAt = characterUpdatedAt

instance AdminRecord Episode where
    afterUpdate oldEpisode (Entity episodeId episode) = do
        if episodeAreEventsComplete episode && not (episodeAreEventsComplete oldEpisode)
            then finalizeEpisode $ Entity episodeId episode
            -- else return ()
            -- TODO - delete the below stuff and replace with the above line
            else if episodeAreEventsComplete oldEpisode && not (episodeAreEventsComplete episode)
                then unfinalizeEpisode $ Entity episodeId episode
                else return ()

    createdBy = episodeCreatedBy
    createdAt = episodeCreatedAt
    updatedBy = episodeUpdatedBy
    updatedAt = episodeUpdatedAt

instance AdminRecord Event where
    afterCreate = changeEventRelations Nothing
    afterUpdate oldEvent = changeEventRelations $ Just oldEvent
    beforeDelete = deleteEventRelations

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
    afterCreate = createLeagueSeasons
    afterUpdate = updateLeagueSeasonsIfRelevent
    createdBy = seriesCreatedBy
    createdAt = seriesCreatedAt
    updatedBy = seriesUpdatedBy
    updatedAt = seriesUpdatedAt

instance AdminRecord Species where
    createdBy = speciesCreatedBy
    createdAt = speciesCreatedAt
    updatedBy = speciesUpdatedBy
    updatedAt = speciesUpdatedAt

