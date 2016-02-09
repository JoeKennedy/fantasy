module Handler.League.ConfirmSettings where

import Import
import Handler.League.Setup

getSetupConfirmSettingsR :: Handler Html
getSetupConfirmSettingsR = do
    userId <- requireAuthId
    let action = SetupLeagueR SetupConfirmSettingsR
    (Entity _ league, lastCompletedStep) <- leagueOrRedirect userId action
    defaultLayout $ do
        let widget = $(widgetFile "league/confirm_setup")
            enctype = UrlEncoded
        setTitle $ leagueSetupStepTitle league action
        $(widgetFile "layouts/league-setup-layout")

postSetupConfirmSettingsR :: Handler Html
postSetupConfirmSettingsR = do
    userId <- requireAuthId
    let action = SetupLeagueR SetupConfirmSettingsR
    (Entity leagueId league, _) <- leagueOrRedirect userId action
    updateLeagueLastCompletedStep leagueId league 6
    redirect LeaguesR

