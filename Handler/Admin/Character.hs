module Handler.Admin.Character where

import Import

import Handler.Admin
import Handler.Character

-----------
-- Forms --
-----------
characterForm :: UserId -> Maybe Character -> Form Character
characterForm userId character = renderBootstrapForm $ Character
    <$> areq textField (fieldName "Name") (characterName <$> character)
    <*> areq textField (fieldName "Bio") (characterBio <$> character)
    <*> areq (selectField speciesList) (fieldName "Species") (characterSpeciesId <$> character)
    <*> aopt (selectField houses) (fieldName "House") (characterHouseId <$> character)
    <*> areq (selectField optionsEnum) (fieldName "Status") (characterStatus <$> character)
    <*> areq intField (fieldName "Season 5 Score") (characterPointsLastSeason <$> character)
    <*> areq intField (fieldName "Episode Count") (characterEpisodesAppearedIn <$> character)
    <*> areq (selectField seriesList) (fieldName "Rookie Season") (characterRookieSeriesId <$> character)
    <*> areq checkBoxField (fieldName "Is Playable") (Just $ fromMaybe True $ characterIsPlayable <$> character)
    <*> createdByField userId (characterCreatedBy <$> character)
    <*> createdAtField (characterCreatedAt <$> character)
    <*> updatedByField userId
    <*> updatedAtField
    where speciesList = optionsPersistKey [] [Asc SpeciesId] speciesName
          houses      = optionsPersistKey [] [Asc HouseName] houseName
          seriesList  = optionsPersistKey [] [Desc SeriesNumber] seriesNumberText


------------
-- Routes --
------------
getAdminCharactersR :: Handler Html
getAdminCharactersR = adminCharactersR

postAdminCharactersR :: Handler Html
postAdminCharactersR = adminCharactersR

getAdminCharacterR :: CharacterId -> Handler Html
getAdminCharacterR = adminCharacterR

postAdminCharacterR :: CharacterId -> Handler Html
postAdminCharacterR = adminCharacterR

deleteAdminCharacterR :: CharacterId -> Handler ()
deleteAdminCharacterR characterId = do
    -- For now, only allow characters to be deleted if they have only blurbs,
    -- events, or players. Otherwise, one of those will trigger a foreign key
    -- failure and halt the transaction block.
    runDB $ do deleteWhere [BlurbCharacterId ==. characterId]
               deleteWhere [EventCharacterId ==. characterId]
               deleteWhere [EventReceivingCharacterId ==. Just characterId]
               deleteWhere [PlayerCharacterId ==. characterId]
    adminDelete "Character" characterId


-------------
-- Helpers --
-------------
adminCharactersR :: Handler Html
adminCharactersR = do
    userId              <- requireAuthId
    characters          <- getAllCharacters
    let form = characterForm userId Nothing
    adminList "character" form $(widgetFile "admin/entities/characters")

adminCharacterR :: CharacterId -> Handler Html
adminCharacterR characterId = do
    userId    <- requireAuthId
    character <- runDB $ get404 characterId
    blurbs    <- getCharacterBlurbs characterId
    let form = characterForm userId $ Just character
        blurbsTable = Just ("blurb", $(widgetFile "admin/entities/blurbs"))
    adminShow "character" form (Entity characterId character) blurbsTable

