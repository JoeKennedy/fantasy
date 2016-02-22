{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

import Import
import Model

import Control.Monad.Logger        (runStderrLoggingT)
import Database.Persist.Postgresql (pgConnStr, withPostgresqlConn, runSqlConn, rawExecute)
import Text.Read

insertSpecies :: ReaderT SqlBackend Handler (SpeciesId)
insertSpecies = insert $ Species "Human" "People like you and I"

insertHouse :: ReaderT SqlBackend Handler (HouseId)
insertHouse = insert $ House "Stark" "Winter is coming" "They are honorable"

insertCharacter :: (Text, Text, SpeciesId, Maybe HouseId) -> ReaderT SqlBackend Handler ()
insertCharacter (name, bio, speciesId, maybeHouseId) =
    insert_ $ Character name bio speciesId maybeHouseId

insertEpisode :: (Text, Int, Int, String, Int) -> ReaderT SqlBackend Handler ()
insertEpisode (name, number, overallNumber, airTime, seriesNumber) = do
    (Entity seriesId _) <- getBy404 $ UniqueSeriesNumber seriesNumber
    insert_ $ Episode name number overallNumber (read airTime :: UTCTime) seriesId

characters :: SpeciesId -> Maybe HouseId -> [(Text, Text, SpeciesId, Maybe HouseId)]
characters firstSpecies firstHouse =
    [ ("Ned Stark", "Gets his head chopped off", firstSpecies, firstHouse)
    , ("Catelyn Stark", "Was resurrected in the books", firstSpecies, firstHouse)
    , ("Robb Stark", "Stabbed in the heart, then beheaded", firstSpecies, firstHouse)
    , ("Jon Snow", "Stabbed in the heart a bunch of times", firstSpecies, firstHouse)
    , ("Sansa Stark", "Just jumped off a castle", firstSpecies, firstHouse)
    , ("Arya Stark", "Becoming a Faceless Man", firstSpecies, firstHouse)
    , ("Bran Stark", "Crippled but learning to fly", firstSpecies, firstHouse)
    , ("Rickon Stark", "Who the hell knows where he is", firstSpecies, firstHouse)
    ]

episodes :: [(Text, Int, Int, String, Int)]
episodes = 
    [ ("Winter Is Coming", 1, 1, "2011-04-18 01:00:00.000000 UTC", 1)
    , ("The Kingsroad", 2, 2, "2011-04-25 01:00:00.000000 UTC", 1)
    , ("Lord Snow", 3, 3, "2011-05-02 01:00:00.000000 UTC", 1)
    , ("Cripples, Bastards, and Broken Things", 4, 4, "2011-05-09 01:00:00.000000 UTC", 1)
    , ("The Wolf and the Lion", 5, 5, "2011-05-16 01:00:00.000000 UTC", 1)
    , ("A Golden Crown", 6, 6, "2011-05-23 01:00:00.000000 UTC", 1)
    , ("You Win or You Die", 7, 7, "2011-05-30 01:00:00.000000 UTC", 1)
    , ("The Pointy End", 8, 8, "2011-06-06 01:00:00.000000 UTC", 1)
    , ("Baelor", 9, 9, "2011-06-13 01:00:00.000000 UTC", 1)
    , ("Fire and Blood", 10, 10, "2011-06-20 01:00:00.000000 UTC", 1)
    ]

main = do
    settings <- loadAppSettingsArgs [configSettingsYmlValue] useEnv
    let conn = (pgConnStr $ appDatabaseConf settings)
    runStderrLoggingT . withPostgresqlConn conn $ runSqlConn $ do
        runMigration migrateAll
        speciesId <- insertSpecies
        houseId <- insertHouse
        mapM_ insertCharacter $ characters speciesId (Just houseId)
        mapM_ (\x -> insert_ $ Series x) [1,2,3,4,5]
        mapM_ insertEpisode episodes

