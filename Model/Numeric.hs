module Model.Numeric where

import ClassyPrelude.Yesod

import Data.Number.CReal

displayRational :: Rational -> String
displayRational num = showCReal 2 (fromRational num)

rank :: [a] -> [(Int, a)]
rank = zip [1..]

rankFirst :: [(a, b)] -> [(Int, a, b)]
rankFirst = map (\(n, (x, y)) -> (n, x, y)) . rank
