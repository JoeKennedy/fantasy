module Handler.Admin.Common where

import Import

import Handler.Admin

------------
-- Routes --
------------
getAdminDashboardR :: Handler Html
getAdminDashboardR = do
    recordCounts <- runDB $ do
        blurbCount     <- count ([] :: [Filter Blurb])
        characterCount <- count ([] :: [Filter Character])
        episodeCount   <- count ([] :: [Filter Episode])
        eventCount     <- count ([] :: [Filter Event])
        houseCount     <- count ([] :: [Filter House])
        seriesCount    <- count ([] :: [Filter Series])
        speciesCount   <- count ([] :: [Filter Species])
        return [ ("blurb",   blurbCount),   ("character", characterCount)
               , ("episode", episodeCount), ("event",     eventCount)
               , ("house",   houseCount),   ("series",    seriesCount)
               , ("species", speciesCount) ]
    let maxCount = foldr (\acc x -> max acc x) 0 $ map snd recordCounts
    adminLayout "" "Dashboard" $(widgetFile "admin/dashboard")


-------------
-- Helpers --
-------------
progressBarAttributes :: Integral a => a -> a -> (Float, Text)
progressBarAttributes number total =
    let width = (log $ fromIntegral number) * 100 / (log $ fromIntegral total)
    in  (width, progressBarColor width)

progressBarColor :: Float -> Text
progressBarColor width
    | width <= 25 = "info"
    | width <= 50 = "success"
    | width <= 75 = "warning"
    | otherwise   = "danger"

