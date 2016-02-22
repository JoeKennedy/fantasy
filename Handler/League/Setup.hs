module Handler.League.Setup where

import Import

import Text.Blaze          (toMarkup)
import Text.Blaze.Internal (Markup)

-------------------------
-- League Setup Wizard --
-------------------------
leagueSetupWizard :: Int -> Int -> [(Int, String, Route App, String)]
leagueSetupWizard lastCompletedStep currentStep =
    map (\(num, name, route) -> (num, name, route, leagueSetupStepClass lastCompletedStep currentStep num)) leagueSetupSteps

leagueSetupStepClass :: Int -> Int -> Int -> String
leagueSetupStepClass lastCompletedStep currentStep stepNumber =
    let relativeClass = case currentStep `compare` stepNumber
                            of GT -> "previous"
                               EQ -> "active"
                               LT -> "next"
        completedClass = case (lastCompletedStep + 1) `compare` stepNumber
                             of LT -> "disabled"
                                _  -> "completed"
    in  relativeClass ++ " " ++ completedClass

leagueSetupCurrentStep :: Route App -> Int
leagueSetupCurrentStep currentRoute =
    case find (\(_, _, stepRoute) -> stepRoute == currentRoute) leagueSetupSteps
        of Just (stepNum, _, _) -> stepNum
           Nothing              -> 1

leagueSetupCurrentStepName :: Route App -> String
leagueSetupCurrentStepName currentRoute =
    case find (\(_, _, stepRoute) -> stepRoute == currentRoute) leagueSetupSteps
        of Just (_, stepName, _) -> stepName
           Nothing               -> error "Step not found"

leagueSetupNextStepToComplete :: League -> Route App
leagueSetupNextStepToComplete league =
    case find (\(stepNum, _, _) -> stepNum == leagueLastCompletedStep league + 1) leagueSetupSteps of
        Just (_, _, stepRoute) -> stepRoute
        Nothing                -> error "No next step"

leagueSetupSteps :: [(Int, String, Route App)]
leagueSetupSteps = [ (1, "Create League",    SetupLeagueR SetupNewLeagueR)
                   , (2, "General Settings", SetupLeagueR SetupGeneralSettingsR)
                   , (3, "Scoring Settings", SetupLeagueR SetupScoringSettingsR)
                   , (4, "Draft Settings",   SetupLeagueR SetupDraftSettingsR)
                   , (5, "Team Settings",    SetupLeagueR SetupTeamsSettingsR)
                   , (6, "Complete Setup",   SetupLeagueR SetupConfirmSettingsR)
                   ]

leagueSetupStepTitle :: League -> Route App -> Markup
leagueSetupStepTitle league currentRoute =
    toMarkup $ "League: " ++ leagueName league ++ " | " ++ pack (leagueSetupCurrentStepName currentRoute)

leagueSetupNextStep :: Int -> Maybe (Int, String, Route App)
leagueSetupNextStep currentStep =
    find (\(stepNum, _, _) -> stepNum == currentStep + 1) leagueSetupSteps

leagueSetupPreviousStep :: Int -> Maybe (Int, String, Route App)
leagueSetupPreviousStep currentStep =
    find (\(stepNum, _, _) -> stepNum == currentStep - 1) leagueSetupSteps

leagueOrRedirect :: UserId -> Route App -> Handler (Entity League, Int)
leagueOrRedirect userId action = do
    maybeLeague <- leagueBeingSetUp userId
    case maybeLeague of
        Nothing -> redirect $ SetupLeagueR SetupNewLeagueR
        Just (Entity leagueId league) ->
            if leagueLastCompletedStep league + 1 >= leagueSetupCurrentStep action
                then return (Entity leagueId league, leagueLastCompletedStep league)
                else redirect $ leagueSetupNextStepToComplete league

leagueBeingSetUp :: UserId -> Handler (Maybe (Entity League))
leagueBeingSetUp userId = runDB $ selectFirst [LeagueCreatedBy ==. userId, LeagueIsSetupComplete ==. False] []

updateLeagueLastCompletedStep :: LeagueId -> League -> Int -> Handler ()
updateLeagueLastCompletedStep leagueId league stepNumber =
    let lastCompletedStep = max (leagueLastCompletedStep league) stepNumber
        isSetupComplete = lastCompletedStep == 6
    in  runDB $ update leagueId [LeagueLastCompletedStep =. lastCompletedStep, LeagueIsSetupComplete =. isSetupComplete]

