module Model.Bootstrap where

import Model

import           ClassyPrelude.Yesod
import           Data.Char             (isLetter, isSpace)
import qualified Data.Text as Text     (toLower, replace)
import           Data.UUID             (UUID)
import           Data.UUID.V4          (nextRandom)
import           Yesod.Form.Bootstrap3 ( BootstrapFormLayout (..)
                                       , BootstrapGridOptions (..)
                                       , renderBootstrap3)

-------------------
--- Bootstrap 3 ---
-------------------
renderBootstrapForm :: (Monad m) => FormRender m a
renderBootstrapForm = renderBootstrap3 defaultBootstrapForm

renderBootstrapPanelForm :: (Monad m) => FormRender m a
renderBootstrapPanelForm = renderBootstrap3 BootstrapBasicForm

defaultBootstrapForm :: BootstrapFormLayout
defaultBootstrapForm = BootstrapHorizontalForm (ColSm 0) (ColSm 2) (ColSm 0) (ColSm 10)

fieldId :: Text -> Text
fieldId name = Text.toLower $ Text.replace " " "_" $ filter (\c -> isLetter c || isSpace c) name

fieldName :: Text -> FieldSettings site
fieldName name = fieldNameWithPlaceholder name ""

fieldNameWithPlaceholder :: Text -> Text -> FieldSettings site
fieldNameWithPlaceholder name fieldPlaceholder =
    FieldSettings (SomeMessage name) Nothing (Just (fieldId name)) Nothing
                  [("class", "form-control"), ("placeholder", fieldPlaceholder)]

formControl :: FieldSettings site
formControl = FieldSettings (SomeMessage ("" :: Text)) Nothing Nothing Nothing [("class", "form-control")]

hidden :: Text -> FieldSettings site
hidden name = FieldSettings (SomeMessage ("" :: Text)) Nothing (Just (fieldId name)) Nothing [("class", "hidden")]

inputSm :: FieldSettings site
inputSm = inputSmClass ""

inputSmClass :: Text -> FieldSettings site
inputSmClass klass = FieldSettings (SomeMessage ("" :: Text)) Nothing Nothing Nothing [("class", "form-control input-sm " ++ klass)]

inputSmHidden :: Text -> FieldSettings site
inputSmHidden text = FieldSettings (SomeMessage text) Nothing Nothing Nothing [("class", "form-control input-sm hidden")]

inputRight :: FieldSettings site
inputRight = FieldSettings (SomeMessage ("" :: Text)) Nothing Nothing Nothing [("class", "form-control text-right input-sm")]

placeholder :: Text -> FieldSettings site
placeholder text = FieldSettings (SomeMessage ("" :: Text)) Nothing Nothing Nothing
                     [("class", "form-control"), ("placeholder", text)]

---------------------
--- Custom Fields ---
---------------------

-- TODO - figure out what type of Applicative are used for forms
createdByField :: (Applicative f) => UserId -> Maybe UserId -> f UserId
createdByField currentUserId maybeCreatedBy = existingElseDefault currentUserId maybeCreatedBy

createdAtField :: (Applicative (t m), MonadTrans t, MonadIO m) => Maybe UTCTime -> t m UTCTime
createdAtField (Just createdAt) = pure createdAt
createdAtField Nothing          = updatedAtField

updatedByField :: (Applicative f) => UserId -> f UserId
updatedByField = pure

updatedAtField :: (Applicative (t m), MonadTrans t, MonadIO m) => t m UTCTime
updatedAtField = lift $ liftIO getCurrentTime

uuidField :: (Applicative (t m), MonadIO m, MonadTrans t) => Maybe UUID -> t m UUID
uuidField (Just uuid) = pure uuid
uuidField Nothing     = lift $ liftIO nextRandom

existingElseDefault :: (Applicative f) => a -> Maybe a -> f a
existingElseDefault defaultValue maybeExistingValue = pure $ fromMaybe defaultValue maybeExistingValue

