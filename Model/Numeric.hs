module Model.Numeric where

import ClassyPrelude.Yesod

import Data.Number.CReal

displayRational :: Rational -> String
displayRational num = showCReal 2 (fromRational num)
