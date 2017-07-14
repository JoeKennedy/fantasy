module Handler.Character where

import Import

import Handler.Event         (getCharacterEvents)
import Handler.League.Player (blurbPanel)
import Handler.League.Season (createPlayerInfo, createPlayerSeasonAndPerformances,
                              maybeCreatePlayerSeason_)

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
    characters <- getCharacters True
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
addCharacterToLeagues :: Entity Character -> Handler ()
addCharacterToLeagues characterEntity = do
    leagueIds <- runDB $ selectKeysList [LeagueIsActive ==. True] [Asc LeagueId]
    forM_ leagueIds $ createPlayerInfo characterEntity

updateCharacterInLeagues :: Character -> Entity Character -> Handler ()
updateCharacterInLeagues oldCharacter (Entity characterId character) = do
    if characterIsPlayable oldCharacter == characterIsPlayable character then return () else
        runDB $ updateWhere [PlayerCharacterId ==. characterId]
                            [PlayerIsPlayable =. characterIsPlayable character]
    players <- runDB $ selectList [PlayerCharacterId ==. characterId] [Asc PlayerId]
    let (playerIds, userId) = (map entityKey players, characterUpdatedBy character)
    performancesCount <- runDB $ count [PerformancePlayerId <-. playerIds]
    case performancesCount of 0 -> forM_ players $ createPlayerSeasonAndPerformances userId
                              _ -> forM_ players $ maybeCreatePlayerSeason_ userId


-------------
-- Widgets --
-------------
charactersTable :: [FullCharacter] -> Widget
charactersTable fullCharacters = $(widgetFile "characters_table")
