module Handler.Series where

import Import

import Handler.Common  (groupByFirst)
import Handler.Episode (getEpisodes)

import Text.Blaze (toMarkup)

------------
-- Routes --
------------
getSeriesListR :: Handler Html
getSeriesListR = do
    episodesAndSeries <- getEpisodes
    let seriesList = groupByFirst episodesAndSeries
    defaultLayout $ do
        setTitle "Game Of Thrones Seasons"
        $(widgetFile "series_list")

getSeriesR :: Int -> Handler Html
getSeriesR seriesNo = do
    Entity seriesId series <- runDB $ getBy404 $ UniqueSeriesNumber seriesNo
    episodes <- runDB $ selectList [EpisodeSeriesId ==. seriesId] [Asc EpisodeNumber]
    defaultLayout $ do
        setTitle $ toMarkup $ "Game Of Thrones Season " ++ show seriesNo
        let episodesPanelTitle = "Episodes" :: String
        $(widgetFile "series")


-------------
-- Widgets --
-------------
seriesEpisodesPanel :: Series -> [Entity Episode] -> String -> Widget
seriesEpisodesPanel series episodes panelTitle = $(widgetFile "series_episodes_panel")

