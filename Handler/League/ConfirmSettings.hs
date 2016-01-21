module Handler.League.ConfirmSettings where

import Import
import Handler.League.Setup

getSetupConfirmSettingsR :: Handler Html
getSetupConfirmSettingsR = do
    userId <- requireAuthId
    (Entity _ league, lastCompletedStep) <- leagueOrRedirect userId
    defaultLayout $ do
        let widget = $(widgetFile "league/confirm_setup")
            action = SetupLeagueR SetupConfirmSettingsR
            enctype = UrlEncoded
        setTitle $ leagueSetupStepTitle league action
        $(widgetFile "layouts/league-setup-layout")

postSetupConfirmSettingsR :: Handler Html
postSetupConfirmSettingsR = do
    userId <- requireAuthId
    (Entity leagueId league, _) <- leagueOrRedirect userId
    updateLeagueLastCompletedStep leagueId league 6
    redirect LeaguesR

