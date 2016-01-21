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

leagueSetupSteps :: [(Int, String, Route App)]
leagueSetupSteps = [ (1, "Create League",    SetupLeagueR SetupNewLeagueR)
                   , (2, "General Settings", SetupLeagueR SetupGeneralSettingsR)
                   , (3, "Scoring Settings", SetupLeagueR SetupScoringSettingsR)
                   , (4, "Draft Settings",   SetupLeagueR SetupDraftSettingsR)
                   , (5, "Team Settings",    SetupLeagueR SetupTeamSettingsR)
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

leagueOrRedirect :: ( YesodPersist site, RedirectUrl site (Route App)
                    , YesodPersistBackend site ~ SqlBackend) =>
                    UserId -> HandlerT site IO (Entity League, Int)
leagueOrRedirect userId = do
    maybeLeague <- leagueBeingSetUp userId
    case maybeLeague of (Just (Entity leagueId league)) -> return (Entity leagueId league, leagueLastCompletedStep league)
                        Nothing       -> redirect $ SetupLeagueR SetupNewLeagueR

leagueBeingSetUp :: (YesodPersist site, YesodPersistBackend site ~ SqlBackend) =>
                    UserId -> HandlerT site IO (Maybe (Entity League))
leagueBeingSetUp userId = runDB $ selectFirst [LeagueCreatedBy ==. userId, LeagueIsSetupComplete ==. False] []

updateLeagueLastCompletedStep :: (YesodPersist site, YesodPersistBackend site ~ SqlBackend) =>
                                 LeagueId -> League -> Int -> HandlerT site IO ()
updateLeagueLastCompletedStep leagueId league stepNumber =
    let lastCompletedStep = max (leagueLastCompletedStep league) stepNumber
        isSetupComplete = lastCompletedStep == 6
    in  runDB $ update leagueId [LeagueLastCompletedStep =. lastCompletedStep, LeagueIsSetupComplete =. isSetupComplete]



-------------------------
-- Very Common Helpers --
-------------------------
extractValue :: (Entity t) -> t
extractValue (Entity _ value) = value

extractValueMaybe :: Maybe (Entity t) -> Maybe t
extractValueMaybe (Just (Entity _ value)) = Just value
extractValueMaybe Nothing                 = Nothing
