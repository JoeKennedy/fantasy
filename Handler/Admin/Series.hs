module Handler.Admin.Series where

import Import

import Handler.Admin
import Handler.Episode

-----------
-- Forms --
-----------
seriesForm :: UserId -> Maybe Series -> Form Series
seriesForm userId series = renderBootstrapForm $ Series
    <$> areq intField (fieldName "Number") (seriesNumber <$> series)
    <*> areq intField (fieldName "Year") (seriesYear <$> series)
    <*> createdByField userId (seriesCreatedBy <$> series)
    <*> createdAtField (seriesCreatedAt <$> series)
    <*> updatedByField userId
    <*> updatedAtField


------------
-- Routes --
------------
getAdminSeriesListR :: Handler Html
getAdminSeriesListR = adminSeriesListR

postAdminSeriesListR :: Handler Html
postAdminSeriesListR = adminSeriesListR

getAdminSeriesR :: SeriesId -> Handler Html
getAdminSeriesR = adminSeriesR

postAdminSeriesR :: SeriesId -> Handler Html
postAdminSeriesR = adminSeriesR

deleteAdminSeriesR :: SeriesId -> Handler ()
deleteAdminSeriesR seriesId = do
    runDB $ deleteWhere [EpisodeSeriesId ==. seriesId]
    adminDelete "Series" seriesId


-------------
-- Helpers --
-------------
adminSeriesListR :: Handler Html
adminSeriesListR = do
    userId <- requireAuthId
    seriesList <- runDB $ selectList [] [Asc SeriesNumber]
    let form = seriesForm userId Nothing
    adminList "series" form $(widgetFile "admin/entities/series_list")

adminSeriesR :: SeriesId -> Handler Html
adminSeriesR seriesId = do
    userId   <- requireAuthId
    series   <- runDB $ get404 seriesId
    episodes <- getSeriesEpisodes seriesId
    let form = seriesForm userId $ Just series
        episodesTable = Just ("episode", $(widgetFile "admin/entities/episodes"))
    adminShow "series" form (Entity seriesId series) episodesTable

