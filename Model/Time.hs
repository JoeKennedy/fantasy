module Model.Time where

import ClassyPrelude.Yesod

import Data.Time
import Text.Printf (printf)

displayTime :: Int -> String
displayTime time = printf "%02d" (time `div` 60) ++ ":" ++ printf "%02d" (time `mod` 60)

estTimeZone :: TimeZone
estTimeZone = TimeZone (-300) False "EST"

edtTimeZone :: TimeZone
edtTimeZone = TimeZone (-240) True "EDT"

utcTimeToEasternTime :: UTCTime -> LocalTime
utcTimeToEasternTime utcTime = utcToLocalTime edtTimeZone utcTime

displayAirDate :: UTCTime -> String
displayAirDate utcTime = formatTime defaultTimeLocale "%B %e, %Y" $ utcTimeToEasternTime utcTime

displayUTCDate :: UTCTime -> String
displayUTCDate = formatTime defaultTimeLocale "%a, %b %e"

displayUTCTime :: UTCTime -> String
displayUTCTime = formatTime defaultTimeLocale "%a, %b %e %l:%M %p %Z"

