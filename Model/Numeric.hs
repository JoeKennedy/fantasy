module Model.Numeric where

import ClassyPrelude.Yesod

import Data.Number.CReal

displayRational :: Rational -> String
displayRational num = showCReal 2 (fromRational num)

----------
-- Rank --
----------
rank :: [a] -> [(Int, a)]
rank = zip [1..]

rank2 :: [(a, b)] -> [(Int, a, b)]
rank2 = map (\(n, (x, y)) -> (n, x, y)) . rank

rank3 :: [(a, b, c)] -> [(Int, a, b, c)]
rank3 = map (\(n, (x, y, z)) -> (n, x, y, z)) . rank

rank4 :: [(a, b, c, d)] -> [(Int, a, b, c, d)]
rank4 = map (\(n, (w, x, y, z)) -> (n, w, x, y, z)) . rank

rank5 :: [(a, b, c, d, e)] -> [(Int, a, b, c, d, e)]
rank5 = map (\(n, (v, w, x, y, z)) -> (n, v, w, x, y, z)) . rank

rank6 :: [(a, b, c, d, e, f)] -> [(Int, a, b, c, d, e, f)]
rank6 = map (\(n, (u, v, w, x, y, z)) -> (n, u, v, w, x, y, z)) . rank

rank7 :: [(a, b, c, d, e, f, g)] -> [(Int, a, b, c, d, e, f, g)]
rank7 = map (\(n, (t, u, v, w, x, y, z)) -> (n, t, u, v, w, x, y, z)) . rank


-----------------
-- Conversions --
-----------------
intToOrdinal :: Int -> String
intToOrdinal int
    | int `div` 10 == 1 = show int ++ "th"
    | otherwise = case int `mod` 10 of 1 -> show int ++ "st"
                                       2 -> show int ++ "nd"
                                       3 -> show int ++ "rd"
                                       _ -> show int ++ "th"
