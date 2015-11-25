module Model.Action where

import Prelude
import Yesod

data Action = Appear | Kill | Defeat | Insult | Maim | Injure | Sentence | Torture | Sex | Guilty | Innocent | Clear | Death | Raise | Pour | Drink | Finish
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
isMultiCharacter Maim     = True
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
actionToSplitString Maim     = (" maimed ", " ")
actionToSplitString Injure   = (" injured ", " ")
actionToSplitString Sentence = (" sentenced ", " to death ")
actionToSplitString Torture  = (" tortured ", " ")
actionToSplitString Sex      = (" had sex with ", " ")
actionToSplitString Guilty   = (" found guilty of a crime ", " ")
actionToSplitString Innocent = (" found innocent of a crime ", " ")
actionToSplitString Clear    = (" cleared of a wrongful accusation or conviction ", " ")
actionToSplitString Death    = (" died ", " ")
actionToSplitString Raise    = (" resurrected ", " ")
actionToSplitString Pour     = (" poured a drink ", " ")
actionToSplitString Drink    = (" took a drink ", " ")
actionToSplitString Finish   = (" finished a drink ", " ")

reverseActionString :: Action -> String
reverseActionString Kill     = " killed by "
reverseActionString Defeat   = " defeated in combat by "
reverseActionString Insult   = " insulted by "
reverseActionString Maim     = " maimed by "
reverseActionString Injure   = " injured by "
reverseActionString Sentence = " sentenced to death by "
reverseActionString Torture  = " tortured by "
reverseActionString Sex      = " had sex with "
reverseActionString Raise    = " resurrected by "
reverseActionString _        = error "This action can't be reversed"

