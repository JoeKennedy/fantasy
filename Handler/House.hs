module Handler.House where

import Import
import Handler.Character (charactersTable)
import Handler.Common    (isAdmin, embeddedForm)

import qualified Database.Esqueleto as E
import           Database.Esqueleto ((^.), (?.))
import           Text.Blaze (toMarkup)
import           Yesod.Form.Bootstrap3 (renderBootstrap3)

houseForm :: Maybe House -> Form House
houseForm house = renderBootstrap3 defaultBootstrapForm $ House
    <$> areq textField (fieldName "Name") (houseName <$> house)
    <*> areq textField (fieldName "Words") (houseWords <$> house)
    <*> areq textField (fieldName "Description") (houseDescription <$> house)

getHousesR :: Handler Html
getHousesR = do
    maybeUser <- maybeAuth
    (widget, enctype) <- generateFormPost $ houseForm Nothing
    houses <- runDB
        $ E.select
        $ E.from $ \(house `E.LeftOuterJoin` character) -> do
            E.on $ E.just (house ^. HouseId) E.==. character ^. CharacterHouseId
            E.groupBy (house ^. HouseId, house ^. HouseName, house ^. HouseDescription, house ^. HouseWords)
            E.orderBy [E.asc (house ^. HouseName)]
            return (house, E.count (character ^. CharacterId) :: E.SqlExpr (E.Value Int))
    defaultLayout $ do
        setTitle "Houses"
        let action = HousesR
        $(widgetFile "houses")

postHousesR :: Handler Html
postHousesR = do
    ((result, widget), enctype) <- runFormPost $ houseForm Nothing
    case result of
        FormSuccess house -> do
            houseId <- runDB $ insert house
            redirect $ HouseR houseId
        _ -> defaultLayout $ do
            setTitle "House creation failed"
            let action = HousesR
            $(widgetFile "embedded_form")

getHouseR :: HouseId -> Handler Html
getHouseR houseId = do
    maybeUser         <- maybeAuth
    h                 <- runDB $ get404 houseId
    (widget, enctype) <- generateFormPost $ houseForm $ Just h
    characters        <- runDB
        $ E.select
        $ E.from $ \(character `E.InnerJoin` species `E.LeftOuterJoin` house `E.InnerJoin` series) -> do
            E.on $ character ^. CharacterRookieSeriesId E.==. series ^. SeriesId
            E.on $ E.just (character ^. CharacterHouseId) E.==. E.just (house ?. HouseId)
            E.on $ character ^. CharacterSpeciesId E.==. species ^. SpeciesId
            E.where_ $ character ^. CharacterHouseId E.==. E.val (Just houseId)
            return (character, species, house, series)
    defaultLayout $ do
        setTitle $ toMarkup $ houseName h
        let action = HouseR houseId
        $(widgetFile "house")

postHouseR :: HouseId -> Handler Html
postHouseR houseId = do
    house <- runDB $ get404 houseId
    ((result, widget), enctype) <- runFormPost $ houseForm $ Just house
    case result of
        FormSuccess house' -> do
            runDB $ replace houseId house'
            redirect $ HouseR houseId
        _ -> defaultLayout $ do
            setTitle "Edit of house failed"
            let action = HouseR houseId
            $(widgetFile "embedded_form")

