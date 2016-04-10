module Handler.Character where

import Import

import Handler.League (createPlayer)

import qualified Database.Esqueleto as E
import           Database.Esqueleto ((^.), (?.))
import           Text.Blaze (toMarkup)
import           Yesod.Form.Bootstrap3 (renderBootstrap3)

characterForm :: UserId -> Maybe Character -> Form Character
characterForm userId character = renderBootstrap3 defaultBootstrapForm $ Character
    <$> areq textField (fieldName "Name") (characterName <$> character)
    <*> areq textField (fieldName "Bio") (characterBio <$> character)
    <*> areq (selectField speciesList) (fieldName "Species") (characterSpeciesId <$> character)
    <*> aopt (selectField houses) (fieldName "House") (characterHouseId <$> character)
    <*> areq (selectField optionsEnum) (fieldName "Status") (characterStatus <$> character)
    <*> createdByField userId (characterCreatedBy <$> character)
    <*> createdAtField (characterCreatedAt <$> character)
    <*> updatedByField userId
    <*> updatedAtField
    where speciesList = optionsPersistKey [] [Asc SpeciesName] speciesName
          houses = optionsPersistKey [] [Asc HouseName] houseName

getCharactersR :: Handler Html
getCharactersR = do
    characters <- runDB
        $ E.select
        $ E.from $ \(character `E.InnerJoin` species `E.LeftOuterJoin` house) -> do
            E.on $ E.just (character ^. CharacterHouseId) E.==. E.just (house ?. HouseId)
            E.on $ character ^. CharacterSpeciesId E.==. species ^. SpeciesId
            E.orderBy [E.asc (character ^. CharacterName)]
            return (character, species, house)
    defaultLayout $ do
      setTitle "Character list"
      $(widgetFile "characters")

getCharacterR :: CharacterId -> Handler Html
getCharacterR characterId = do
    -- later try to make this get by character name if possible
    character  <- runDB $ get404 characterId
    species    <- runDB $ get404 $ characterSpeciesId character
    maybeHouse <- runDB $ mapM get $ characterHouseId character
    events     <- runDB
        $ E.select
        $ E.from $ \(event `E.InnerJoin` episode `E.InnerJoin` series `E.InnerJoin` actingCharacter `E.LeftOuterJoin` receivingCharacter) -> do
            E.on $ E.just (event ^. EventReceivingCharacterId) E.==. E.just (receivingCharacter ?. CharacterId)
            E.on $ event ^. EventCharacterId E.==. actingCharacter ^. CharacterId
            E.on $ episode ^. EpisodeSeriesId E.==. series ^. SeriesId
            E.on $ event ^. EventEpisodeId E.==. episode ^. EpisodeId
            E.where_ (event ^. EventCharacterId E.==. E.val characterId E.||. event ^. EventReceivingCharacterId E.==. E.just (E.val characterId))
            E.orderBy [E.asc (episode ^. EpisodeOverallNumber), E.asc (event ^. EventTimeInEpisode)]
            return (event, episode, series, actingCharacter, receivingCharacter)
    defaultLayout $ do
      setTitle $ toMarkup $ characterName character
      $(widgetFile "character")

getNewCharacterR :: Handler Html
getNewCharacterR = do
    userId <- requireAuthId
    (widget, enctype) <- generateFormPost $ characterForm userId Nothing
    defaultLayout $ do
        let title = "Create a new character" :: Html
            action = NewCharacterR
        setTitle title
        $(widgetFile "character_form")

postNewCharacterR :: Handler Html
postNewCharacterR = do
    userId <- requireAuthId
    ((result, widget), enctype) <- runFormPost $ characterForm userId Nothing
    case result of
        FormSuccess character -> do
            characterId <- runDB $ insert character
            leagues <- runDB $ selectList [] [Asc LeagueId]
            mapM_ (\l -> runDB $ createPlayer l characterId) leagues
            redirect $ CharacterR characterId
        _ -> defaultLayout $ do
            let title = "Create a new character" :: Html
                action = NewCharacterR
            setTitle title
            $(widgetFile "character_form")

getEditCharacterR :: CharacterId -> Handler Html
getEditCharacterR characterId = do
    userId <- requireAuthId
    character <- runDB $ get404 characterId
    (widget, enctype) <- generateFormPost $ characterForm userId $ Just character
    defaultLayout $ do
        let title = toMarkup $ "Edit " ++ characterName character
            action = EditCharacterR characterId
        setTitle title
        $(widgetFile "character_form")

postEditCharacterR :: CharacterId -> Handler Html
postEditCharacterR characterId = do
    userId <- requireAuthId
    character <- runDB $ get404 characterId
    ((result, widget), enctype) <- runFormPost $ characterForm userId $ Just character
    case result of
        FormSuccess character' -> do
            runDB $ replace characterId character'
            redirect $ CharacterR characterId
        _ -> defaultLayout $ do
            let title = toMarkup $ "Edit " ++ characterName character
                action = EditCharacterR characterId
            setTitle title
            $(widgetFile "character_form")


-------------
-- Helpers --
-------------
type FullCharacter = (Entity Character, Entity Species, Maybe (Entity House))

characterList :: [FullCharacter] -> Widget
characterList characters = $(widgetFile "characters")
