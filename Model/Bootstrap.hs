module Model.Bootstrap where

import qualified Data.Text as Text (toLower, replace)
import ClassyPrelude.Yesod
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), BootstrapGridOptions (..))

defaultBootstrapForm :: BootstrapFormLayout
defaultBootstrapForm = BootstrapHorizontalForm (ColSm 0) (ColSm 2) (ColSm 0) (ColSm 10)

fieldId :: Text -> Text
fieldId name = Text.toLower $ Text.replace " " "_" name

fieldName :: Text -> FieldSettings site
fieldName name = FieldSettings (SomeMessage name) Nothing (Just (fieldId name)) Nothing [("class", "form-control")]

