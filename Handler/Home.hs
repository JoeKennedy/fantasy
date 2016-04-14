module Handler.Home where

import Import

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
    maybeUserId <- maybeAuthId
    seriesList <- runDB $ selectList [] [Asc SeriesNumber]
    leagues <- getLeaguesByUser maybeUserId
    defaultLayout $ do
        setTitle "Fantasy Game Of Thrones"
        $(widgetFile "homepage")

getFAQR :: Handler Html
getFAQR = defaultLayout $ do
    setTitle "Fantasy Game Of Thrones | How To Play"
    $(widgetFile "faq")
