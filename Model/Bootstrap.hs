module Model.Bootstrap where

import Model

import           Data.Char         (isLetter, isSpace)
import qualified Data.Text as Text (toLower, replace)
import ClassyPrelude.Yesod
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), BootstrapGridOptions (..), renderBootstrap3)

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
fieldName name = FieldSettings (SomeMessage name) Nothing (Just (fieldId name)) Nothing [("class", "form-control")]

formControl :: FieldSettings site
formControl = FieldSettings (SomeMessage ("" :: Text)) Nothing Nothing Nothing [("class", "form-control")]

hidden :: Text -> FieldSettings site
hidden name = FieldSettings (SomeMessage ("" :: Text)) Nothing (Just (fieldId name)) Nothing [("class", "hidden")]

inputSm :: FieldSettings site
inputSm = FieldSettings (SomeMessage ("" :: Text)) Nothing Nothing Nothing [("class", "form-control input-sm")]

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
updatedByField currentUserId = pure currentUserId

updatedAtField :: (Applicative (t m), MonadTrans t, MonadIO m) => t m UTCTime
updatedAtField = lift (liftIO getCurrentTime)

existingElseDefault :: (Applicative f) => a -> Maybe a -> f a
existingElseDefault defaultValue maybeExistingValue = pure (fromMaybe defaultValue maybeExistingValue)

