module Handler.Character where

import Import

import Handler.Event         (getCharacterEvents)
import Handler.Common        (isAdmin)
import Handler.League.Player (blurbPanel)
import Handler.League.Week   (createPerformance)

import qualified Database.Esqueleto as E
import           Database.Esqueleto ((^.), (?.))
import           Text.Blaze (toMarkup)

-----------
-- Types --
-----------
type FullCharacter = (Entity Character, Entity Species, Maybe (Entity House), Entity Series)


------------
-- Routes --
------------
getCharactersR :: Handler Html
getCharactersR = do
    maybeUser  <- maybeAuth
    characters <- getCharacters True
    unplayable <- getCharacters False
    defaultLayout $ do
      setTitle "Character list"
      $(widgetFile "characters")

getCharacterR :: CharacterId -> Handler Html
getCharacterR characterId = do
    maybeUser  <- maybeAuth
    character  <- runDB $ get404 characterId
    species    <- runDB $ get404 $ characterSpeciesId character
    maybeHouse <- runDB $ mapM get $ characterHouseId character
    blurbs     <- runDB $ selectList [BlurbCharacterId ==. characterId] [Desc BlurbId]
    events     <- getCharacterEvents characterId
    rookieSeries <- runDB $ get404 $ characterRookieSeriesId character
    defaultLayout $ do
      setTitle $ toMarkup $ characterName character
      $(widgetFile "character")


-------------
-- Queries --
-------------
getAllCharacters :: Handler [FullCharacter]
getAllCharacters = runDB
    $ E.select
    $ E.from $ \(character `E.InnerJoin` species `E.LeftOuterJoin` house `E.InnerJoin` series) -> do
        E.on $ character ^. CharacterRookieSeriesId E.==. series ^. SeriesId
        E.on $ E.just (character ^. CharacterHouseId) E.==. E.just (house ?. HouseId)
        E.on $ character ^. CharacterSpeciesId E.==. species ^. SpeciesId
        E.orderBy [E.asc (character ^. CharacterName)]
        return (character, species, house, series)

getCharacters :: Bool -> Handler [FullCharacter]
getCharacters isPlayable = runDB
    $ E.select
    $ E.from $ \(character `E.InnerJoin` species `E.LeftOuterJoin` house `E.InnerJoin` series) -> do
        E.on $ character ^. CharacterRookieSeriesId E.==. series ^. SeriesId
        E.on $ E.just (character ^. CharacterHouseId) E.==. E.just (house ?. HouseId)
        E.on $ character ^. CharacterSpeciesId E.==. species ^. SpeciesId
        E.where_ $ character ^. CharacterIsPlayable E.==. E.val isPlayable
        E.orderBy [E.asc (character ^. CharacterName)]
        return (character, species, house, series)

getBlurbs :: Handler [(Entity Blurb, Entity Character)]
getBlurbs = runDB
    $ E.select
    $ E.from $ \(blurb `E.InnerJoin` character) -> do
        E.on $ blurb ^. BlurbCharacterId E.==. character ^. CharacterId
        E.orderBy [E.desc (blurb ^. BlurbId)]
        return (blurb, character)

getCharacterBlurbs :: CharacterId -> Handler [(Entity Blurb, Entity Character)]
getCharacterBlurbs characterId = runDB
    $ E.select
    $ E.from $ \(blurb `E.InnerJoin` character) -> do
        E.on $ blurb ^. BlurbCharacterId E.==. character ^. CharacterId
        E.where_ $ blurb ^. BlurbCharacterId E.==. E.val characterId
        E.orderBy [E.desc (blurb ^. BlurbId)]
        return (blurb, character)


---------------
-- Callbacks --
---------------
createCharacterPerformancesIfNecessary :: CharacterId -> Handler ()
createCharacterPerformancesIfNecessary characterId = do
    playerIds <- runDB $ selectKeysList [PlayerCharacterId ==. characterId] [Asc PlayerId]
    performances <- runDB $ count [PerformancePlayerId <-. playerIds]
    case performances of 0 -> createCharacterPerformances characterId
                         _ -> return ()

createCharacterPerformances :: CharacterId -> Handler ()
createCharacterPerformances characterId = do
    weeks <- runDB $ selectList [] [Asc WeekId]
    mapM_ (createCharacterPerformanceForWeek characterId) weeks

createCharacterPerformanceForWeek :: CharacterId -> Entity Week -> Handler ()
createCharacterPerformanceForWeek characterId (Entity weekId week) = do
    let leagueId = weekLeagueId week
    previousWeekIds <- runDB $ selectKeysList [WeekNumber <. weekNumber week] []
    playerEntity <- runDB $ getBy404 $ UniquePlayerLeagueIdCharacterId leagueId characterId
    createPerformance leagueId weekId previousWeekIds playerEntity


-------------
-- Widgets --
-------------
charactersTable :: [FullCharacter] -> Widget
charactersTable fullCharacters = $(widgetFile "characters_table")
