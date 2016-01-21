module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi
import Data.Time.LocalTime (TimeOfDay)
import Model.Action
import Model.Types

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkSave "entityDefs"]
    $(persistFileWith lowerCaseSettings "config/models")
