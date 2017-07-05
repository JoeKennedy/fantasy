module Model.Action where

import Model.Types

import           ClassyPrelude.Yesod
import qualified Data.Char           as Char

data Action = Appear
            | Advise
            | Affection
            | Attack
            | Bravery
            | Clear
            | Command
            | Cowardice
            | Death
            | Defeat
            | Dishonor
            | Drink
            | Fight
            | Finish
            | Force
            | Gain
            | Guilty
            | Honor
            | Injure
            | Innocent
            | Insult
            | Kill
            | Loss
            | Magic
            | Manipulate
            | Massacre
            | Onslaught
            | Order
            | Pour
            | Praise
            | Quest
            | Raise
            | Sacrifice
            | Sentence
            | Sex
            | Suicide
            | Tease
            | Threaten
            | Torture
            | Wit
    deriving (Show, Read, Eq, Enum, Bounded)
derivePersistField "Action"

allActions :: [Action]
allActions = [minBound .. maxBound] :: [Action]

multiCharacterActions :: [String]
multiCharacterActions = map show $ filter isMultiCharacter allActions

isMultiCharacter :: Action -> Bool
isMultiCharacter Appear    = False
isMultiCharacter Advise    = True
isMultiCharacter Affection = True
isMultiCharacter Attack    = True
isMultiCharacter Bravery   = False
isMultiCharacter Clear     = False
isMultiCharacter Command   = True
isMultiCharacter Cowardice = False
isMultiCharacter Death     = False
isMultiCharacter Defeat    = True
isMultiCharacter Dishonor  = False
isMultiCharacter Drink     = False
isMultiCharacter Fight     = True
isMultiCharacter Finish    = False
isMultiCharacter Force     = True
isMultiCharacter Gain      = False
isMultiCharacter Guilty    = False
isMultiCharacter Honor     = False
isMultiCharacter Injure    = True
isMultiCharacter Innocent  = False
isMultiCharacter Insult    = True
isMultiCharacter Kill      = True
isMultiCharacter Loss      = False
isMultiCharacter Magic     = False
isMultiCharacter Manipulate= True
isMultiCharacter Massacre  = False
isMultiCharacter Onslaught = False
isMultiCharacter Order     = False
isMultiCharacter Pour      = False
isMultiCharacter Praise    = True
isMultiCharacter Quest     = False
isMultiCharacter Raise     = True
isMultiCharacter Sacrifice = False
isMultiCharacter Sentence  = True
isMultiCharacter Sex       = True
isMultiCharacter Suicide   = False
isMultiCharacter Tease     = True
isMultiCharacter Threaten  = True
isMultiCharacter Torture   = True
isMultiCharacter Wit       = False

actionToSplitString :: Action -> (String, String)
actionToSplitString Appear    = ("appeared in the episode ", "")
actionToSplitString Advise    = ("gave advice to ", "")
actionToSplitString Affection = ("displayed affection with ", "")
actionToSplitString Attack    = ("attacked ", "")
actionToSplitString Bravery   = ("displayed bravery ", "")
actionToSplitString Clear     = ("was cleared of a wrongful charge ", "")
actionToSplitString Command   = ("issued a command to ", "")
actionToSplitString Cowardice = ("displayed cowardice ", "")
actionToSplitString Death     = ("died of natural causes ", "")
actionToSplitString Defeat    = ("defeated ", " in combat ")
actionToSplitString Dishonor  = ("acted dishonorably ", "")
actionToSplitString Drink     = ("took an alcoholic drink ", "")
actionToSplitString Fight     = ("fought ", "")
actionToSplitString Finish    = ("finished an alcoholic drink ", "")
actionToSplitString Force     = ("forced sex upon ", "")
actionToSplitString Gain      = ("learned of positive development ", "")
actionToSplitString Guilty    = ("was found guilty of a crime ", "")
actionToSplitString Honor     = ("acted honorably ", "")
actionToSplitString Injure    = ("injured ", "")
actionToSplitString Innocent  = ("was found innocent of a crime ", "")
actionToSplitString Insult    = ("insulted ", "")
actionToSplitString Kill      = ("killed ", "")
actionToSplitString Loss      = ("learned of negative development ", "")
actionToSplitString Magic     = ("performed a feat of magic ", "")
actionToSplitString Manipulate= ("manipulated ", "")
actionToSplitString Massacre  = ("massacred a number of characters ", "")
actionToSplitString Onslaught = ("launched a mass attack ", "")
actionToSplitString Order     = ("ordered subordinates ", "")
actionToSplitString Pour      = ("poured an alcoholic drink ", "")
actionToSplitString Praise    = ("praised ", "")
actionToSplitString Quest     = ("completed a long personal quest ", "")
actionToSplitString Raise     = ("raised ", " from the dead ")
actionToSplitString Sacrifice = ("sacrificed life ", "")
actionToSplitString Sentence  = ("sentenced ", " to death ")
actionToSplitString Sex       = ("had sex with ", "")
actionToSplitString Suicide   = ("committed suicide ", "")
actionToSplitString Tease     = ("teased ", "")
actionToSplitString Threaten  = ("threatened ", "")
actionToSplitString Torture   = ("tortured ", "")
actionToSplitString Wit       = ("dropped a witty line ", "")

actionToSplitCaps :: Action -> (String, String)
actionToSplitCaps action =
    let (before, after) = actionToSplitString action
    in  (capitalize before, after)

reverseActionString :: Action -> String
reverseActionString Advise    = "advised by"
reverseActionString Attack    = "attacked by"
reverseActionString Affection = "displayed affection with"
reverseActionString Command   = "issued a command by"
reverseActionString Defeat    = "defeated in combat by"
reverseActionString Fight     = "fought"
reverseActionString Force     = "was forced to have sex with"
reverseActionString Injure    = "injured by"
reverseActionString Insult    = "insulted by"
reverseActionString Kill      = "killed by"
reverseActionString Manipulate= "manipulated by"
reverseActionString Praise    = "praised by"
reverseActionString Raise     = "resurrected by"
reverseActionString Sentence  = "sentenced to death by"
reverseActionString Sex       = "had sex with"
reverseActionString Tease     = "teased by"
reverseActionString Threaten  = "threatened by"
reverseActionString Torture   = "tortured by"
reverseActionString _         = error "This is not a multi-player action"

reverseActionCaps :: Action -> String
reverseActionCaps = capitalize . reverseActionString

-- (enabled by default, points, weight, receiving points, receiving weight)
weightedScoringAttributes :: Action -> (Bool, Int, Int, Int, Int)
weightedScoringAttributes Appear    = (False,  1,   0,   0,   0)
weightedScoringAttributes Advise    = (True,   6,   3,   2,   1)
weightedScoringAttributes Affection = (True,   6,   3,   6,   3)
weightedScoringAttributes Attack    = (True,   5,   2,  -5,  -2)
weightedScoringAttributes Bravery   = (True,  10,   0,   0,   0)
weightedScoringAttributes Clear     = (True,  14,   0,   0,   0)
weightedScoringAttributes Command   = (True,   2,   0,   0,   0)
weightedScoringAttributes Cowardice = (True, -10,   0,   0,   0)
weightedScoringAttributes Death     = (True,  -5,  -2,   0,   0)
weightedScoringAttributes Defeat    = (True,  15,   7, -15,  -7)
weightedScoringAttributes Dishonor  = (True,  -5,   0,   0,   0)
weightedScoringAttributes Drink     = (True,   4,   0,   0,   0)
weightedScoringAttributes Fight     = (True,  10,   5,  10,   5)
weightedScoringAttributes Finish    = (True,   6,   0,   0,   0)
weightedScoringAttributes Force     = (False,  0,   0,   0,   0)
weightedScoringAttributes Gain      = (True,   5,   0,   0,   0)
weightedScoringAttributes Guilty    = (True,  -7,   0,   0,   0)
weightedScoringAttributes Honor     = (True,   5,   0,   0,   0)
weightedScoringAttributes Injure    = (True,  10,   5, -10,  -5)
weightedScoringAttributes Innocent  = (True,   7,   0,   0,   0)
weightedScoringAttributes Insult    = (True,   5,   2,  -5,  -2)
weightedScoringAttributes Kill      = (True,  20,  10, -20, -10)
weightedScoringAttributes Loss      = (True,  -5,   0,   0,   0)
weightedScoringAttributes Magic     = (True,  20,   0,   0,   0)
weightedScoringAttributes Manipulate= (True,  10,   5, -10,  -5)
weightedScoringAttributes Massacre  = (True, 100,   0,   0,   0)
weightedScoringAttributes Onslaught = (True,  25,   0,   0,   0)
weightedScoringAttributes Order     = (True,   8,   0,   0,   0)
weightedScoringAttributes Pour      = (True,   2,   0,   0,   0)
weightedScoringAttributes Praise    = (True,   5,   2,   5,   2)
weightedScoringAttributes Quest     = (True,  25,   0,   0,   0)
weightedScoringAttributes Raise     = (True,  20,  10,  20,  10)
weightedScoringAttributes Sacrifice = (True,  20,  10,   0,   0)
weightedScoringAttributes Sentence  = (True,  10,   5, -10,  -5)
weightedScoringAttributes Sex       = (True,  12,   6,  12,   6)
weightedScoringAttributes Suicide   = (True, -20, -10,   0,   0)
weightedScoringAttributes Tease     = (True,   3,   1,   0,   0)
weightedScoringAttributes Threaten  = (True,   7,   3,  -7,  -3)
weightedScoringAttributes Torture   = (True,  16,   8, -16,  -8)
weightedScoringAttributes Wit       = (True,   3,   0,   0,   0)

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

capitalizeText :: Text -> Text
capitalizeText text = case uncons text of Just (x, xs) -> cons (Char.toUpper x) xs
                                          Nothing      -> text
