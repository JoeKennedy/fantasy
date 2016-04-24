module Handler.Species where

import Import
import Handler.Character (charactersTable)
import Handler.Common    (isAdmin, embeddedForm)

import qualified Database.Esqueleto as E
import           Database.Esqueleto ((^.), (?.))
import           Text.Blaze (toMarkup)
import           Yesod.Form.Bootstrap3 (renderBootstrap3)

speciesForm :: Maybe Species -> Form Species
speciesForm species = renderBootstrap3 defaultBootstrapForm $ Species
    <$> areq textField (fieldName "Name") (speciesName <$> species)
    <*> areq textField (fieldName "Description") (speciesDescription <$> species)

getSpeciesListR :: Handler Html
getSpeciesListR = do
    maybeUser <- maybeAuth
    (widget, enctype) <- generateFormPost $ speciesForm Nothing
    species_list <- runDB
        $ E.select
        $ E.from $ \(species `E.LeftOuterJoin` character) -> do
            E.on $ species ^. SpeciesId E.==. character ^. CharacterSpeciesId
            E.groupBy (species ^. SpeciesId, species ^. SpeciesName, species ^. SpeciesDescription)
            E.orderBy [E.asc (species ^. SpeciesName)]
            return (species, E.count (character ^. CharacterId) :: E.SqlExpr (E.Value Int))
    defaultLayout $ do
        setTitle "Species list"
        let action = SpeciesListR
        $(widgetFile "species_list")

postSpeciesListR :: Handler Html
postSpeciesListR = do
    ((result, widget), enctype) <- runFormPost $ speciesForm Nothing
    case result of
        FormSuccess species -> do
            speciesId <- runDB $ insert species
            redirect $ SpeciesR speciesId
        _ -> defaultLayout $ do
            setTitle "Species creation failed"
            let action = SpeciesListR
            $(widgetFile "embedded_form")

getSpeciesR :: SpeciesId -> Handler Html
getSpeciesR speciesId = do
    maybeUser         <- maybeAuth
    s                 <- runDB $ get404 speciesId
    (widget, enctype) <- generateFormPost $ speciesForm $ Just s
    characters        <- runDB
        $ E.select
        $ E.from $ \(character `E.InnerJoin` species `E.LeftOuterJoin` house `E.InnerJoin` series) -> do
            E.on $ character ^. CharacterRookieSeriesId E.==. series ^. SeriesId
            E.on $ E.just (character ^. CharacterHouseId) E.==. E.just (house ?. HouseId)
            E.on $ character ^. CharacterSpeciesId E.==. species ^. SpeciesId
            E.where_ $ character ^. CharacterSpeciesId E.==. E.val speciesId
                 E.&&. character ^. CharacterIsPlayable E.==. E.val True
            E.orderBy [E.asc (character ^. CharacterName)]
            return (character, species, house, series)
    defaultLayout $ do
        setTitle $ toMarkup $ speciesName s
        let action = SpeciesR speciesId
        $(widgetFile "species")

postSpeciesR :: SpeciesId -> Handler Html
postSpeciesR speciesId = do
    species <- runDB $ get404 speciesId
    ((result, widget), enctype) <- runFormPost $ speciesForm $ Just species
    case result of
        FormSuccess species' -> do
            runDB $ replace speciesId species'
            redirect $ SpeciesR speciesId
        _ -> defaultLayout $ do
            setTitle "Edit of species failed"
            let action = SpeciesR speciesId
            $(widgetFile "embedded_form")


