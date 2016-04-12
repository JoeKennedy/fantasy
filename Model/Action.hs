module Model.Action where

import Model.Types

import ClassyPrelude.Yesod

data Action = Appear | Kill | Defeat | Insult | Attack | Injure | Sentence | Torture | Sex | Force | Guilty | Innocent | Clear | Death | Raise | Pour | Drink | Finish
    deriving (Show, Read, Eq, Enum, Bounded)
derivePersistField "Action"

allActions :: [Action]
allActions = [minBound .. maxBound] :: [Action]

multiCharacterActions :: [String]
multiCharacterActions = map show $ filter isMultiCharacter allActions

isMultiCharacter :: Action -> Bool
isMultiCharacter Appear   = False
isMultiCharacter Kill     = True
isMultiCharacter Defeat   = True
isMultiCharacter Insult   = True
isMultiCharacter Attack   = True
isMultiCharacter Injure   = True
isMultiCharacter Sentence = True
isMultiCharacter Torture  = True
isMultiCharacter Sex      = True
isMultiCharacter Force    = True
isMultiCharacter Guilty   = False
isMultiCharacter Innocent = False
isMultiCharacter Clear    = False
isMultiCharacter Death    = False
isMultiCharacter Raise    = True
isMultiCharacter Pour     = False
isMultiCharacter Drink    = False
isMultiCharacter Finish   = False

actionToSplitString :: Action -> (String, String)
actionToSplitString Appear   = (" appeared in the episode ", " ")
actionToSplitString Kill     = (" killed ", " ")
actionToSplitString Defeat   = (" defeated ", " in combat ")
actionToSplitString Insult   = (" insulted ", " ")
actionToSplitString Attack   = (" attacked ", " ")
actionToSplitString Injure   = (" injured ", " ")
actionToSplitString Sentence = (" sentenced ", " to death ")
actionToSplitString Torture  = (" tortured ", " ")
actionToSplitString Sex      = (" had sex with ", " ")
actionToSplitString Force    = (" forced sex upon ", " ")
actionToSplitString Guilty   = (" was found guilty of a crime ", " ")
actionToSplitString Innocent = (" was found innocent of a crime ", " ")
actionToSplitString Clear    = (" was cleared of a wrongful charge ", " ")
actionToSplitString Death    = (" died of natural causes ", " ")
actionToSplitString Raise    = (" raised ", " from the dead ")
actionToSplitString Pour     = (" poured an alcoholic drink ", " ")
actionToSplitString Drink    = (" took an alcoholic drink ", " ")
actionToSplitString Finish   = (" finished an alcoholic drink ", " ")

reverseActionString :: Action -> String
reverseActionString Kill     = " killed by "
reverseActionString Defeat   = " defeated in combat by "
reverseActionString Insult   = " insulted by "
reverseActionString Attack   = " attacked by "
reverseActionString Injure   = " injured by "
reverseActionString Sentence = " sentenced to death by "
reverseActionString Torture  = " tortured by "
reverseActionString Sex      = " had sex with "
reverseActionString Force    = " was forced to have sex with "
reverseActionString Raise    = " resurrected by "
reverseActionString _        = error "This is not a multi-player action"

weightedScoringAttributes :: Action -> (Bool, Int, Int, Int, Int)
weightedScoringAttributes Appear   = (False,  0,   0,   0,   0)
weightedScoringAttributes Kill     = (True,  20,  20, -20, -20)
weightedScoringAttributes Defeat   = (True,  15,  15, -15, -15)
weightedScoringAttributes Insult   = (True,   3,   3,  -3,  -3)
weightedScoringAttributes Attack   = (True,   5,   5,  -5,  -5)
weightedScoringAttributes Injure   = (True,  10,  10, -10, -10)
weightedScoringAttributes Sentence = (True,  18,  18, -18, -18)
weightedScoringAttributes Torture  = (True,  16,  16, -16, -16)
weightedScoringAttributes Sex      = (True,  12,  12,  12,  12)
weightedScoringAttributes Force    = (True,   6,   6,  -6,  -6)
weightedScoringAttributes Guilty   = (True,  -7,  -7,   0,   0)
weightedScoringAttributes Innocent = (True,   7,   7,   0,   0)
weightedScoringAttributes Clear    = (True,  14,  14,   0,   0)
weightedScoringAttributes Death    = (True,  -5,  -5,   0,   0)
weightedScoringAttributes Raise    = (True,  20,  20,  20,  20)
weightedScoringAttributes Pour     = (True,   1,   0,   0,   0)
weightedScoringAttributes Drink    = (True,   2,   0,   0,   0)
weightedScoringAttributes Finish   = (True,   4,   0,   0,   0)

defaultScoringAttributes :: Action -> ScoringType -> (Bool, Int, Int, Int, Int)
defaultScoringAttributes action Weighted = weightedScoringAttributes action
defaultScoringAttributes action Vanilla =
    let (isUsed, points, _, pointsRec, _) = weightedScoringAttributes action
    in  (isUsed, points, 0, pointsRec, 0)
defaultScoringAttributes _ Scorekeeper = error "This isn't a thing"

