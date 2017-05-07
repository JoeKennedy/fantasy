module Handler.Admin.Blurb where

import Import

import Handler.Admin
import Handler.Character

-----------
-- Forms --
-----------
blurbForm :: UserId -> Maybe Blurb -> Form Blurb
blurbForm userId blurb = renderBootstrapForm $ Blurb
    <$> areq (selectField characters) (fieldName "Character") (blurbCharacterId <$> blurb)
    <*> areq textareaField (fieldName "Content") (blurbContent <$> blurb)
    <*> createdByField userId (blurbCreatedBy <$> blurb)
    <*> createdAtField (blurbCreatedAt <$> blurb)
    <*> updatedByField userId
    <*> updatedAtField
    where characters = optionsPersistKey [] [Asc CharacterName] characterName


------------
-- Routes --
------------
getAdminBlurbsR :: Handler Html
getAdminBlurbsR = adminBlurbsR

postAdminBlurbsR :: Handler Html
postAdminBlurbsR = adminBlurbsR

getAdminBlurbR :: BlurbId -> Handler Html
getAdminBlurbR = adminBlurbR

postAdminBlurbR :: BlurbId -> Handler Html
postAdminBlurbR = adminBlurbR

deleteAdminBlurbR :: BlurbId -> Handler ()
deleteAdminBlurbR = adminDelete "Blurb"


-------------
-- Helpers --
-------------
adminBlurbsR :: Handler Html
adminBlurbsR = do
    userId <- requireAuthId
    blurbs <- getBlurbs
    let form = blurbForm userId Nothing
    adminList "blurb" form $(widgetFile "admin/entities/blurbs")

adminBlurbR :: BlurbId -> Handler Html
adminBlurbR blurbId = do
    userId <- requireAuthId
    blurb  <- runDB $ get404 blurbId
    let form = blurbForm userId $ Just blurb
    adminShow "blurb" form (Entity blurbId blurb) Nothing

