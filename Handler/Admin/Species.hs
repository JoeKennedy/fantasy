module Handler.Admin.Species where

import Import

import Handler.Admin
import Handler.Species

import qualified Database.Esqueleto as E

-----------
-- Forms --
-----------
speciesForm :: UserId -> Maybe Species -> Form Species
speciesForm userId species = renderBootstrapForm $ Species
    <$> areq textField (fieldName "Name") (speciesName <$> species)
    <*> areq textField (fieldName "Description") (speciesDescription <$> species)
    <*> createdByField userId (speciesCreatedBy <$> species)
    <*> createdAtField (speciesCreatedAt <$> species)
    <*> updatedByField userId
    <*> updatedAtField


------------
-- Routes --
------------
getAdminSpeciesListR :: Handler Html
getAdminSpeciesListR = adminSpeciesListR

postAdminSpeciesListR :: Handler Html
postAdminSpeciesListR = adminSpeciesListR

getAdminSpeciesR :: SpeciesId -> Handler Html
getAdminSpeciesR = adminSpeciesR

postAdminSpeciesR :: SpeciesId -> Handler Html
postAdminSpeciesR = adminSpeciesR

deleteAdminSpeciesR :: SpeciesId -> Handler ()
deleteAdminSpeciesR speciesId = do
    runDB $ deleteWhere [CharacterSpeciesId ==. speciesId]
    adminDelete "Species" speciesId


-------------
-- Helpers --
-------------
adminSpeciesListR :: Handler Html
adminSpeciesListR = do
    userId      <- requireAuthId
    speciesList <- getSpeciesList
    let form = speciesForm userId Nothing
    adminList "species" form $(widgetFile "admin/entities/species_list")

adminSpeciesR :: SpeciesId -> Handler Html
adminSpeciesR speciesId = do
    userId     <- requireAuthId
    species    <- runDB $ get404 speciesId
    characters <- getSpeciesCharacters speciesId
    let form = speciesForm userId $ Just species
        charactersTable = Just ("character", $(widgetFile "admin/entities/characters"))
    adminShow "species" form (Entity speciesId species) charactersTable

