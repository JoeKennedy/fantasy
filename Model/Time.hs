module Model.Time where

import Prelude
import Data.Time
import Data.Time.Format

estTimeZone :: TimeZone
estTimeZone = TimeZone (-300) False "EST"

edtTimeZone :: TimeZone
edtTimeZone = TimeZone (-240) True "EDT"

utcTimeToEasternTime :: UTCTime -> LocalTime
utcTimeToEasternTime utcTime = utcToLocalTime edtTimeZone utcTime

displayAirDate :: UTCTime -> String
displayAirDate utcTime = formatTime defaultTimeLocale "%B %e, %Y" $ utcTimeToEasternTime utcTime

