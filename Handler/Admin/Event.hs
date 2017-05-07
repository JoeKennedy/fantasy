module Handler.Admin.Event where

import Import

import Handler.Admin
import Handler.Episode
import Handler.Event

-----------
-- Forms --
-----------
eventForm :: UserId -> Maybe Event -> Form Event
eventForm userId event = renderBootstrapForm $ Event
    <$> areq (selectField characters)  (fieldName "Character") (eventCharacterId <$> event)
    <*> areq (selectField optionsEnum) (fieldName "Action") (eventAction <$> event)
    <*> aopt (selectField characters)  (fieldName "Receiving Character") (eventReceivingCharacterId <$> event)
    <*> areq (selectField episodes)    (fieldName "Episode") (eventEpisodeId <$> event)
    <*> aopt textField (fieldName "Further description") (eventNote <$> event)
    <*> areq timeInEpisodeField (fieldName "Time in episode") (eventTimeInEpisode <$> event)
    <*> createdByField userId (eventCreatedBy <$> event)
    <*> createdAtField (eventCreatedAt <$> event)
    <*> updatedByField userId
    <*> updatedAtField
    where characters = optionsPersistKey [] [Asc CharacterName] characterName
          episodes   = optionsPersistKey [] [Desc EpisodeOverallNumber] episodeToText


------------
-- Routes --
------------
getAdminEventsR :: Handler Html
getAdminEventsR = adminEventsR

postAdminEventsR :: Handler Html
postAdminEventsR = adminEventsR

getAdminEventR :: EventId -> Handler Html
getAdminEventR = adminEventR

postAdminEventR :: EventId -> Handler Html
postAdminEventR = adminEventR

deleteAdminEventR :: EventId -> Handler ()
deleteAdminEventR = adminDelete "Event"


-------------
-- Helpers --
-------------
adminEventsR :: Handler Html
adminEventsR = do
    userId <- requireAuthId
    events <- getEvents
    let form = eventForm userId Nothing
    adminList "event" form $(widgetFile "admin/entities/events")

adminEventR :: EventId -> Handler Html
adminEventR eventId = do
    userId <- requireAuthId
    event  <- runDB $ get404 eventId
    let form = eventForm userId $ Just event
    adminShow "event" form (Entity eventId event) Nothing

