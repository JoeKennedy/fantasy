module Handler.House where

import Import

import Handler.Character (FullCharacter, charactersTable)

import qualified Database.Esqueleto as E
import           Database.Esqueleto ((^.), (?.))
import           Text.Blaze (toMarkup)

------------
-- Routes --
------------
getHousesR :: Handler Html
getHousesR = do
    houses <- getHouses
    defaultLayout $ do
        setTitle "Houses"
        $(widgetFile "houses")

getHouseR :: HouseId -> Handler Html
getHouseR houseId = do
    h                 <- runDB $ get404 houseId
    characters        <- getHouseCharacters houseId
    defaultLayout $ do
        setTitle $ toMarkup $ houseName h
        $(widgetFile "house")


-------------
-- Queries --
-------------
getHouses :: Handler [(Entity House, E.Value Int)]
getHouses = runDB
    $ E.select
    $ E.from $ \(house `E.LeftOuterJoin` character) -> do
        E.on $ E.just (house ^. HouseId) E.==. character ^. CharacterHouseId
        E.groupBy (house ^. HouseId, house ^. HouseName, house ^. HouseDescription, house ^. HouseWords)
        E.orderBy [E.asc (house ^. HouseName)]
        return (house, E.count (character ^. CharacterId) :: E.SqlExpr (E.Value Int))

getHouseCharacters :: HouseId -> Handler [FullCharacter]
getHouseCharacters houseId = runDB
        $ E.select
        $ E.from $ \(character `E.InnerJoin` species `E.LeftOuterJoin` house `E.InnerJoin` series) -> do
            E.on $ character ^. CharacterRookieSeriesId E.==. series ^. SeriesId
            E.on $ E.just (character ^. CharacterHouseId) E.==. E.just (house ?. HouseId)
            E.on $ character ^. CharacterSpeciesId E.==. species ^. SpeciesId
            E.where_ $ character ^. CharacterHouseId E.==. E.val (Just houseId)
                 E.&&. character ^. CharacterIsPlayable E.==. E.val True
            E.orderBy [E.asc (character ^. CharacterName)]
            return (character, species, house, series)
