module Handler.League.GeneralSettings where

import Import
import Handler.League.Setup
import Handler.League.Layout

----------
-- Form --
----------
generalSettingsForm :: Int -> UserId -> GeneralSettings -> Html -> MForm Handler (FormResult GeneralSettings, Widget)
generalSettingsForm teamsCount currentUserId generalSettings extra = do
    (startersRes, startersView) <- mreq (selectFieldList $ toOptions possibleNumbersOfStarters)
        (fieldName "Maximum number of starters") (Just $ generalSettingsNumberOfStarters generalSettings)
    (rosterSizeRes, rosterSizeView) <- mreq (selectFieldList $ toOptions possibleRosterSizes)
        (fieldName "Maximum roster size") (Just $ generalSettingsRosterSize generalSettings)
    (regSeasonLengthRes, regSeasonLengthView) <- mreq (selectFieldList $ toOptions possibleRegularSeasonLengths)
        (fieldName "Regular Season length (in weeks)") (Just $ generalSettingsRegularSeasonLength generalSettings)
    (playoffLengthRes, playoffLengthView) <- mreq (selectFieldList $ toOptions possiblePlayoffLengths)
        (fieldName "Playoff length (in weeks)") (Just $ generalSettingsPlayoffLength generalSettings)
    (teamsInPlayoffsRes, teamsInPlayoffsView) <- mreq (selectFieldList $ toOptions (possibleNumbersOfTeamsInPlayoffs teamsCount))
        (fieldName "Number of teams in playoffs") (Just $ generalSettingsNumberOfTeamsInPlayoffs generalSettings)
    (tradeDeadlineWeekRes, tradeDeadlineWeekView) <- mreq (selectFieldList $ toOptions possibleTradeDeadlineWeeks)
        (fieldName "Trade deadline week") (Just $ generalSettingsTradeDeadlineWeek generalSettings)
    (waiverPeriodRes, waiverPeriodView) <- mreq (selectFieldList $ toOptions possibleWaiverPeriodsInDays)
        (fieldName "Waiver Period (in days)") (Just $ generalSettingsWaiverPeriodInDays generalSettings)

    now <- liftIO getCurrentTime
    let generalSettingsResult = GeneralSettings
            <$> pure (generalSettingsLeagueId generalSettings)
            <*> startersRes
            <*> rosterSizeRes
            <*> regSeasonLengthRes
            <*> playoffLengthRes
            <*> teamsInPlayoffsRes
            <*> tradeDeadlineWeekRes
            <*> waiverPeriodRes
            <*> pure (generalSettingsCreatedBy generalSettings)
            <*> pure (generalSettingsCreatedAt generalSettings)
            <*> updatedByField currentUserId
            <*> pure now
    return (generalSettingsResult, $(widgetFile "league/general_settings_form"))

------------
-- Routes --
------------
getSetupGeneralSettingsR :: Handler Html
getSetupGeneralSettingsR = do
    userId <- requireAuthId
    (Entity leagueId league, lastCompletedStep) <- leagueOrRedirect userId
    Entity _ generalSettings <- runDB $ getBy404 $ UniqueGeneralSettingsLeagueId leagueId
    (widget, enctype) <- generateFormPost $ generalSettingsForm (leagueTeamsCount league) userId generalSettings
    defaultLayout $ do
        let action = SetupLeagueR SetupGeneralSettingsR
        setTitle $ leagueSetupStepTitle league action
        $(widgetFile "layouts/league-setup-layout")

postSetupGeneralSettingsR :: Handler Html
postSetupGeneralSettingsR = do
    userId <- requireAuthId
    (Entity leagueId league, lastCompletedStep) <- leagueOrRedirect userId
    Entity generalSettingsId generalSettings <- runDB $ getBy404 $ UniqueGeneralSettingsLeagueId leagueId
    ((result, widget), enctype) <- runFormPost $ generalSettingsForm (leagueTeamsCount league) userId generalSettings
    case result of
        FormSuccess generalSettings' -> do
            runDB $ replace generalSettingsId generalSettings'
            updateLeagueLastCompletedStep leagueId league 2
            redirect $ SetupLeagueR SetupScoringSettingsR
        _ -> defaultLayout $ do
            let action = SetupLeagueR SetupGeneralSettingsR
            setTitle $ leagueSetupStepTitle league action
            $(widgetFile "layouts/league-setup-layout")

getLeagueGeneralSettingsR :: LeagueId -> Handler Html
getLeagueGeneralSettingsR leagueId = do
    userId <- requireAuthId
    league <- runDB $ get404 leagueId
    Entity _ generalSettings <- runDB $ getBy404 $ UniqueGeneralSettingsLeagueId leagueId
    (widget, enctype) <- generateFormPost $ generalSettingsForm (leagueTeamsCount league) userId generalSettings
    let action = LeagueSettingsR leagueId LeagueGeneralSettingsR
    leagueSettingsLayout leagueId action enctype widget "General"

postLeagueGeneralSettingsR :: LeagueId -> Handler Html
postLeagueGeneralSettingsR leagueId = do
    userId <- requireAuthId
    league <- runDB $ get404 leagueId
    Entity generalSettingsId generalSettings <- runDB $ getBy404 $ UniqueGeneralSettingsLeagueId leagueId
    ((result, widget), enctype) <- runFormPost $ generalSettingsForm (leagueTeamsCount league) userId generalSettings
    let action = LeagueSettingsR leagueId LeagueGeneralSettingsR
    case result of
        FormSuccess generalSettings' -> do
            runDB $ replace generalSettingsId generalSettings'
            setMessage "Successfully updated league general settings"
            redirect action
        _ -> leagueSettingsLayout leagueId action enctype widget "General"

