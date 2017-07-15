module Model.Time where

import ClassyPrelude.Yesod

import Data.Time
import Data.Time.Calendar.WeekDate
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

displayAirTime :: UTCTime -> String
displayAirTime utcTime = formatTime defaultTimeLocale "%B %e, %Y at %l:%M %p EDT" $ utcTimeToEasternTime utcTime

displayUTCDate :: UTCTime -> String
displayUTCDate = formatTime defaultTimeLocale "%a, %b %e, %Y"

displayUTCTime :: UTCTime -> String
displayUTCTime = formatTime defaultTimeLocale "%a, %b %e, %Y %l:%M %p %Z"

addXDays :: Int -> UTCTime -> UTCTime
addXDays x utcTime = UTCTime { utctDay = addDays (toInteger x) (utctDay utcTime)
                             , utctDayTime = utctDayTime utcTime
                             }

dayOfWeek :: UTCTime -> Int
dayOfWeek utcTime = let (_, _, dayOfTheWeek) = toWeekDate $ utctDay utcTime
                    in  dayOfTheWeek `mod` 7

dayOfWeekToText :: Int -> Text
dayOfWeekToText 0 = "Sunday"
dayOfWeekToText 1 = "Monday"
dayOfWeekToText 2 = "Tuesday"
dayOfWeekToText 3 = "Wednesday"
dayOfWeekToText 4 = "Thursday"
dayOfWeekToText 5 = "Friday"
dayOfWeekToText 6 = "Saturday"
dayOfWeekToText x = dayOfWeekToText $ x `mod` 7

past24Hours :: Maybe UTCTime -> IO Bool
past24Hours = map (fromMaybe False) . traverse past24Hrs

past24Hrs :: UTCTime -> IO Bool
past24Hrs utcTime = do
    now <- getCurrentTime
    return $ diffUTCTime now utcTime <= 86400

