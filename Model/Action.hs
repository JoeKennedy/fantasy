module Model.Action where

import Model.Types

import           ClassyPrelude.Yesod
import qualified Data.Char           as Char

data Action = Appear | Kill | Defeat | Insult | Fight | Attack | Injure | Bravery | Cowardice | Sentence | Torture | Sex | Force | Guilty | Innocent | Clear | Death | Raise | Pour | Drink | Finish
    deriving (Show, Read, Eq, Enum, Bounded)
derivePersistField "Action"

allActions :: [Action]
allActions = [minBound .. maxBound] :: [Action]

multiCharacterActions :: [String]
multiCharacterActions = map show $ filter isMultiCharacter allActions

isMultiCharacter :: Action -> Bool
isMultiCharacter Appear    = False
isMultiCharacter Kill      = True
isMultiCharacter Defeat    = True
isMultiCharacter Insult    = True
isMultiCharacter Fight     = True
isMultiCharacter Attack    = True
isMultiCharacter Injure    = True
isMultiCharacter Sentence  = True
isMultiCharacter Bravery   = False
isMultiCharacter Cowardice = False
isMultiCharacter Torture   = True
isMultiCharacter Sex       = True
isMultiCharacter Force     = True
isMultiCharacter Guilty    = False
isMultiCharacter Innocent  = False
isMultiCharacter Clear     = False
isMultiCharacter Death     = False
isMultiCharacter Raise     = True
isMultiCharacter Pour      = False
isMultiCharacter Drink     = False
isMultiCharacter Finish    = False

actionToSplitString :: Action -> (String, String)
actionToSplitString Appear    = ("appeared in the episode ", "")
actionToSplitString Kill      = ("killed ", "")
actionToSplitString Defeat    = ("defeated ", " in combat ")
actionToSplitString Insult    = ("insulted ", "")
actionToSplitString Fight     = ("fought ", "")
actionToSplitString Attack    = ("attacked ", "")
actionToSplitString Injure    = ("injured ", "")
actionToSplitString Sentence  = ("sentenced ", " to death ")
actionToSplitString Bravery   = ("displayed bravery ", "")
actionToSplitString Cowardice = ("displayed cowardice ", "")
actionToSplitString Torture   = ("tortured ", "")
actionToSplitString Sex       = ("had sex with ", "")
actionToSplitString Force     = ("forced sex upon ", "")
actionToSplitString Guilty    = ("was found guilty of a crime ", "")
actionToSplitString Innocent  = ("was found innocent of a crime ", "")
actionToSplitString Clear     = ("was cleared of a wrongful charge ", "")
actionToSplitString Death     = ("died of natural causes ", "")
actionToSplitString Raise     = ("raised ", " from the dead ")
actionToSplitString Pour      = ("poured an alcoholic drink ", "")
actionToSplitString Drink     = ("took an alcoholic drink ", "")
actionToSplitString Finish    = ("finished an alcoholic drink ", "")

actionToSplitCaps :: Action -> (String, String)
actionToSplitCaps action =
    let (before, after) = actionToSplitString action
    in  (capitalize before, after)

reverseActionString :: Action -> String
reverseActionString Kill      = "killed by"
reverseActionString Defeat    = "defeated in combat by"
reverseActionString Insult    = "insulted by"
reverseActionString Fight     = "fought"
reverseActionString Attack    = "attacked by"
reverseActionString Injure    = "injured by"
reverseActionString Sentence  = "sentenced to death by"
reverseActionString Torture   = "tortured by"
reverseActionString Sex       = "had sex with"
reverseActionString Force     = "was forced to have sex with"
reverseActionString Raise     = "resurrected by"
reverseActionString _         = error "This is not a multi-player action"

reverseActionCaps :: Action -> String
reverseActionCaps = capitalize . reverseActionString

weightedScoringAttributes :: Action -> (Bool, Int, Int, Int, Int)
weightedScoringAttributes Appear    = (False,  0,   0,   0,   0)
weightedScoringAttributes Kill      = (True,  20,  10, -20, -10)
weightedScoringAttributes Defeat    = (True,  15,   7, -15,  -7)
weightedScoringAttributes Insult    = (True,   5,   2,  -5,  -2)
weightedScoringAttributes Fight     = (True,  10,   5,  10,   5)
weightedScoringAttributes Attack    = (True,   5,   2,  -5,  -2)
weightedScoringAttributes Injure    = (True,  10,   5, -10,  -5)
weightedScoringAttributes Sentence  = (True,  18,   9, -18,  -9)
weightedScoringAttributes Bravery   = (True,   5,   2,   0,   0)
weightedScoringAttributes Cowardice = (True,  -5,  -2,   0,   0)
weightedScoringAttributes Torture   = (True,  16,   8, -16,  -8)
weightedScoringAttributes Sex       = (True,  12,   6,  12,   6)
weightedScoringAttributes Force     = (False,  0,   0,   0,   0)
weightedScoringAttributes Guilty    = (True,  -7,  -3,   0,   0)
weightedScoringAttributes Innocent  = (True,   7,   3,   0,   0)
weightedScoringAttributes Clear     = (True,  14,   7,   0,   0)
weightedScoringAttributes Death     = (True,  -5,  -2,   0,   0)
weightedScoringAttributes Raise     = (True,  20,  10,  20,  10)
weightedScoringAttributes Pour      = (True,   1,   0,   0,   0)
weightedScoringAttributes Drink     = (True,   2,   0,   0,   0)
weightedScoringAttributes Finish    = (True,   4,   0,   0,   0)

defaultScoringAttributes :: Action -> ScoringType -> (Bool, Int, Int, Int, Int)
defaultScoringAttributes action Weighted = weightedScoringAttributes action
defaultScoringAttributes action Vanilla =
    let (isUsed, points, _, pointsRec, _) = weightedScoringAttributes action
    in  (isUsed, points, 0, pointsRec, 0)
defaultScoringAttributes _ Scorekeeper = error "This isn't a thing"

-------------
-- Helpers --
-------------
capitalize :: String -> String
capitalize []     = []
capitalize (x:xs) = (Char.toUpper x):xs

