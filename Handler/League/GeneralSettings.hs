module Handler.League.GeneralSettings where

import Import
import Handler.League.Setup
import Handler.League.Layout

----------
-- Form --
----------
generalSettingsForm :: Int -> UserId -> SeasonId -> Series -> GeneralSettings -> Form GeneralSettings
generalSettingsForm teamsCount currentUserId seasonId series generalSettings extra = do
    let totalWeeks = seriesTotalEpisodes series
    (startersRes, startersView) <- mreq (selectFieldList $ toOptions $ possibleNumbersOfStarters teamsCount)
        (fieldName "Maximum number of starters") (Just $ generalSettingsNumberOfStarters generalSettings)
    (rosterSizeRes, rosterSizeView) <- mreq (selectFieldList $ toOptions $ possibleRosterSizes teamsCount)
        (fieldName "Maximum roster size") (Just $ generalSettingsRosterSize generalSettings)
    (regSeasonLengthRes, regSeasonLengthView) <- mreq (selectFieldList $ toOptions $ possibleRegularSeasonLengths totalWeeks)
        (fieldName "Regular Season length (in weeks)") (Just $ generalSettingsRegularSeasonLength generalSettings)
    (playoffLengthRes, playoffLengthView) <- mreq (selectFieldList $ toOptions $ possiblePlayoffLengths totalWeeks)
        (fieldName "Playoff length (in weeks)") (Just $ generalSettingsPlayoffLength generalSettings)
    (teamsInPlayoffsRes, teamsInPlayoffsView) <- mreq (selectFieldList $ toOptions (possibleNumbersOfTeamsInPlayoffs teamsCount))
        (fieldName "Number of teams in playoffs") (Just $ generalSettingsNumberOfTeamsInPlayoffs generalSettings)
    (tradeDeadlineWeekRes, tradeDeadlineWeekView) <- mreq (selectFieldList $ toOptions $ possibleTradeDeadlineWeeks totalWeeks)
        (fieldName "Trade deadline week") (Just $ generalSettingsTradeDeadlineWeek generalSettings)
    (waiverPeriodRes, waiverPeriodView) <- mreq (selectFieldList $ toOptions possibleWaiverPeriodsInDays)
        (fieldName "Waiver Period (in days)") (Just $ generalSettingsWaiverPeriodInDays generalSettings)

    now <- liftIO getCurrentTime
    let generalSettingsResult = GeneralSettings
            <$> pure (generalSettingsLeagueId generalSettings)
            <*> pure seasonId
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
    let action = SetupLeagueR SetupGeneralSettingsR
    (Entity leagueId league, lastCompletedStep) <- leagueOrRedirect userId action
    Entity seasonId season <- getSelectedSeason leagueId
    Entity _ generalSettings <- runDB $ getBy404 $ UniqueGeneralSettingsSeasonId seasonId 
    series <- runDB $ get404 $ seasonSeriesId season
    (widget, enctype) <- generateFormPost $ generalSettingsForm (leagueTeamsCount league) userId seasonId series generalSettings
    defaultLayout $ do
        setTitle $ leagueSetupStepTitle league action
        let maybeLeagueId = Just leagueId
        $(widgetFile "layouts/league-setup-layout")

postSetupGeneralSettingsR :: Handler Html
postSetupGeneralSettingsR = do
    userId <- requireAuthId
    let action = SetupLeagueR SetupGeneralSettingsR
    (Entity leagueId league, lastCompletedStep) <- leagueOrRedirect userId action
    Entity seasonId season <- getSelectedSeason leagueId
    Entity generalSettingsId generalSettings <- runDB $ getBy404 $ UniqueGeneralSettingsSeasonId seasonId
    series <- runDB $ get404 $ seasonSeriesId season
    ((result, widget), enctype) <- runFormPost $ generalSettingsForm (leagueTeamsCount league) userId seasonId series generalSettings
    case result of
        FormSuccess generalSettings' -> do
            runDB $ replace generalSettingsId generalSettings'
            updateLeagueLastCompletedStep leagueId league 2
            redirect $ SetupLeagueR SetupScoringSettingsR
        _ -> defaultLayout $ do
            setTitle $ leagueSetupStepTitle league action
            let maybeLeagueId = Just leagueId
            $(widgetFile "layouts/league-setup-layout")

getLeagueGeneralSettingsR :: LeagueId -> Handler Html
getLeagueGeneralSettingsR leagueId = do
    userId <- requireAuthId
    league <- runDB $ get404 leagueId
    Entity seasonId season <- getSelectedSeason leagueId
    Entity _ generalSettings <- runDB $ getBy404 $ UniqueGeneralSettingsSeasonId seasonId
    series <- runDB $ get404 $ seasonSeriesId season
    (widget, enctype) <- generateFormPost $ generalSettingsForm (leagueTeamsCount league) userId seasonId series generalSettings
    let action = LeagueSettingsR leagueId LeagueGeneralSettingsR
    leagueSettingsLayout leagueId action enctype widget "General"

postLeagueGeneralSettingsR :: LeagueId -> Handler Html
postLeagueGeneralSettingsR leagueId = do
    userId <- requireAuthId
    league <- runDB $ get404 leagueId
    Entity seasonId season <- getSelectedSeason leagueId
    Entity generalSettingsId generalSettings <- runDB $ getBy404 $ UniqueGeneralSettingsSeasonId seasonId
    series <- runDB $ get404 $ seasonSeriesId season
    ((result, widget), enctype) <- runFormPost $ generalSettingsForm (leagueTeamsCount league) userId seasonId series generalSettings
    let action = LeagueSettingsR leagueId LeagueGeneralSettingsR
    case result of
        FormSuccess generalSettings' -> do
            runDB $ replace generalSettingsId generalSettings'
            setMessage "Successfully updated league general settings"
            redirect action
        _ -> leagueSettingsLayout leagueId action enctype widget "General"

