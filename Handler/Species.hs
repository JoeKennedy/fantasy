module Handler.Species where

import Import

import Handler.Character (FullCharacter, charactersTable)

import qualified Database.Esqueleto as E
import           Database.Esqueleto ((^.), (?.))
import           Text.Blaze (toMarkup)

------------
-- Routes --
------------
getSpeciesListR :: Handler Html
getSpeciesListR = do
    speciesList <- getSpeciesList
    defaultLayout $ do
        setTitle "Species list"
        $(widgetFile "species_list")

getSpeciesR :: SpeciesId -> Handler Html
getSpeciesR speciesId = do
    s                 <- runDB $ get404 speciesId
    characters        <- getSpeciesCharacters speciesId
    defaultLayout $ do
        setTitle $ toMarkup $ speciesName s
        $(widgetFile "species")


-------------
-- Queries --
-------------
getSpeciesList :: Handler [(Entity Species, E.Value Int)]
getSpeciesList = runDB
    $ E.select
    $ E.from $ \(species `E.LeftOuterJoin` character) -> do
        E.on $ species ^. SpeciesId E.==. character ^. CharacterSpeciesId
        E.groupBy (species ^. SpeciesId, species ^. SpeciesName, species ^. SpeciesDescription)
        E.orderBy [E.asc (species ^. SpeciesName)]
        return (species, E.count (character ^. CharacterId) :: E.SqlExpr (E.Value Int))

getSpeciesCharacters :: SpeciesId -> Handler [FullCharacter]
getSpeciesCharacters speciesId = runDB
        $ E.select
        $ E.from $ \(character `E.InnerJoin` species `E.LeftOuterJoin` house `E.InnerJoin` series) -> do
            E.on $ character ^. CharacterRookieSeriesId E.==. series ^. SeriesId
            E.on $ E.just (character ^. CharacterHouseId) E.==. E.just (house ?. HouseId)
            E.on $ character ^. CharacterSpeciesId E.==. species ^. SpeciesId
            E.where_ $ character ^. CharacterSpeciesId E.==. E.val speciesId
                 E.&&. character ^. CharacterIsPlayable E.==. E.val True
            E.orderBy [E.asc (character ^. CharacterName)]
            return (character, species, house, series)
