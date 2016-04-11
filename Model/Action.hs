module Model.Action where

import Model.Types

import ClassyPrelude.Yesod

data Action = Appear | Kill | Defeat | Insult | Attack | Injure | Sentence | Torture | Sex | Guilty | Innocent | Clear | Death | Raise | Pour | Drink | Finish
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
reverseActionString Raise    = " resurrected by "
reverseActionString _        = error "This action can't be reversed"

defaultScoringAttributes :: Action -> ScoringType -> (Bool, Int, Int, Int, Int)
defaultScoringAttributes action Weighted
    | isMultiCharacter action == True  = (True, 10, 50, -20, -50)
    | otherwise = (True, 10, 50, 0, 0)
defaultScoringAttributes action Vanilla
    | isMultiCharacter action == True  = (True, 10, 0, -20, 0)
    | otherwise = (True, 10, 0, 0, 0)
defaultScoringAttributes _ Scorekeeper = error "This isn't a thing"

