module Handler.Admin.House where

import Import

import Handler.Admin
import Handler.House

import qualified Database.Esqueleto as E

-----------
-- Forms --
-----------
houseForm :: UserId -> Maybe House -> Form House
houseForm userId house = renderBootstrapForm $ House
    <$> areq textField (fieldName "Name") (houseName <$> house)
    <*> areq textField (fieldName "Words") (houseWords <$> house)
    <*> areq textField (fieldName "Description") (houseDescription <$> house)
    <*> createdByField userId (houseCreatedBy <$> house)
    <*> createdAtField (houseCreatedAt <$> house)
    <*> updatedByField userId
    <*> updatedAtField


------------
-- Routes --
------------
getAdminHousesR :: Handler Html
getAdminHousesR = adminHousesR

postAdminHousesR :: Handler Html
postAdminHousesR = adminHousesR

getAdminHouseR :: HouseId -> Handler Html
getAdminHouseR = adminHouseR

postAdminHouseR :: HouseId -> Handler Html
postAdminHouseR = adminHouseR

deleteAdminHouseR :: HouseId -> Handler ()
deleteAdminHouseR houseId = do
    runDB $ deleteWhere [CharacterHouseId ==. Just houseId]
    adminDelete "House" houseId


-------------
-- Helpers --
-------------
adminHousesR :: Handler Html
adminHousesR = do
    userId <- requireAuthId
    houses <- getHouses
    let form = houseForm userId Nothing
    adminList "house" form $(widgetFile "admin/entities/houses")

adminHouseR :: HouseId -> Handler Html
adminHouseR houseId = do
    userId <- requireAuthId
    house  <- runDB $ get404 houseId
    characters <- getHouseCharacters houseId
    let form = houseForm userId $ Just house
        charactersTable = Just ("character", $(widgetFile "admin/entities/characters"))
    adminShow "house" form (Entity houseId house) charactersTable

