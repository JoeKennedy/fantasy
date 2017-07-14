module Handler.League.DraftSettings where

import Import

import Handler.League.Setup
import Handler.League.Layout

----------
-- Form --
----------
draftSettingsForm :: UserId -> Entity Season -> Maybe DraftSettings -> Form DraftSettings
draftSettingsForm currentUserId (Entity seasonId season) draftSettings extra = do
    let draftOrderTypeOptionsToUse =
            if isJust $ seasonDraftOrderDeterminedAt season
                then selectedDraftOrderTypeOptions (draftSettingsDraftOrderType <$> draftSettings)
                else draftOrderTypeOptions

    (draftTypeRes, draftTypeView) <- mreq hiddenField (hidden "Draft type")
        (draftSettingsDraftType <$> draftSettings)
    (draftOrderRes, draftOrderView) <- mreq (selectFieldList draftOrderOptions)
        (fieldName "Draft Order") (draftSettingsDraftOrder <$> draftSettings)
    (draftOrderTypeRes, draftOrderTypeView) <- mreq (selectFieldList draftOrderTypeOptionsToUse)
        (fieldName "Determination Of Draft Order") (draftSettingsDraftOrderType <$> draftSettings)
    (dateRes, dateView) <- mopt dayField (fieldName "Draft Day")
        (draftSettingsDate <$> draftSettings)
    (timeRes, timeView) <- mopt timeFieldTypeTime (fieldName "Draft Time")
        (draftSettingsTime <$> draftSettings)
    (locationRes, locationView) <- mopt textField (fieldName "Location")
        (draftSettingsLocation <$> draftSettings)
    (allowDraftPickTradingRes, allowDraftPickTradingView) <- mreq checkBoxField
        "Allow draft pick trading?" (draftSettingsAllowDraftPickTrading <$> draftSettings)
    (secondsPerPickRes, secondsPerPickView) <- mreq
        (selectFieldList $ toOptions possibleSecondsPerPick) (fieldName "Seconds Per Pick")
        (existingElseDefault defaultSecondsPerPick $ draftSettingsSecondsPerPick <$> draftSettings)
    (noteRes, noteView) <- mopt textareaField (fieldName "Note")
        (draftSettingsNote <$> draftSettings)

    now <- liftIO getCurrentTime
    let draftSettingsResult = DraftSettings
            <$> pure (seasonLeagueId season)
            <*> pure seasonId
            <*> draftTypeRes
            <*> draftOrderRes
            <*> (if isJust (seasonDraftOrderDeterminedAt season)
                    then pure (fromMaybe ManuallySet (draftSettingsDraftOrderType <$> draftSettings))
                    else draftOrderTypeRes)
            <*> dateRes
            <*> timeRes
            <*> locationRes
            <*> allowDraftPickTradingRes
            <*> secondsPerPickRes
            <*> noteRes
            <*> createdByField currentUserId (draftSettingsCreatedBy <$> draftSettings)
            <*> existingElseDefault now (draftSettingsCreatedAt <$> draftSettings)
            <*> updatedByField currentUserId
            <*> pure now
    return (draftSettingsResult, $(widgetFile "league/draft_settings_form"))


------------
-- Routes --
------------
getSetupDraftSettingsR :: Handler Html
getSetupDraftSettingsR = do
    userId <- requireAuthId
    let action = SetupLeagueR SetupDraftSettingsR
    (Entity leagueId league, lastCompletedStep) <- leagueOrRedirect userId action
    seasonEntity <- getSelectedSeason leagueId
    maybeDraftSettings <- runDB $ getBy $ UniqueDraftSettingsSeasonId $ entityKey seasonEntity
    (widget, enctype) <- generateFormPost $ draftSettingsForm userId seasonEntity $ map entityVal maybeDraftSettings
    defaultLayout $ do
        setTitle $ leagueSetupStepTitle league action
        let maybeLeagueId = Just leagueId
        $(widgetFile "layouts/league-setup-layout")

postSetupDraftSettingsR :: Handler Html
postSetupDraftSettingsR = do
    userId <- requireAuthId
    let action = SetupLeagueR SetupDraftSettingsR
    (Entity leagueId league, lastCompletedStep) <- leagueOrRedirect userId action
    seasonEntity <- getSelectedSeason leagueId
    maybeDraftSettings <- runDB $ getBy $ UniqueDraftSettingsSeasonId $ entityKey seasonEntity
    ((result, widget), enctype) <- runFormPost $ draftSettingsForm userId seasonEntity $ map entityVal maybeDraftSettings
    case result of
        FormSuccess draftSettings -> do
            case maybeDraftSettings of Just (Entity dsId _) -> runDB $ replace dsId draftSettings
                                       Nothing              -> runDB $ insert_ draftSettings
            randomizeDraftOrderIfRelevant userId RandomNow draftSettings seasonEntity
            updateLeagueLastCompletedStep leagueId league 4
            redirect $ SetupLeagueR SetupTeamsSettingsR
        _ -> defaultLayout $ do
            setTitle $ leagueSetupStepTitle league action
            let maybeLeagueId = Just leagueId
            $(widgetFile "layouts/league-setup-layout")

getLeagueDraftSettingsR :: LeagueId -> Handler Html
getLeagueDraftSettingsR leagueId = do
    userId <- requireAuthId
    seasonEntity <- getSelectedSeason leagueId
    maybeDraftSettings <- runDB $ getBy $ UniqueDraftSettingsSeasonId $ entityKey seasonEntity
    (widget, enctype) <- generateFormPost $ draftSettingsForm userId seasonEntity $ map entityVal maybeDraftSettings
    let action = LeagueSettingsR leagueId LeagueDraftSettingsR
    leagueSettingsLayout leagueId action enctype widget "Draft"

postLeagueDraftSettingsR :: LeagueId -> Handler Html
postLeagueDraftSettingsR leagueId = do
    userId <- requireAuthId
    seasonEntity <- getSelectedSeason leagueId
    maybeDraftSettings <- runDB $ getBy $ UniqueDraftSettingsSeasonId $ entityKey seasonEntity
    ((result, widget), enctype) <- runFormPost $ draftSettingsForm userId seasonEntity $ map entityVal maybeDraftSettings
    let action = LeagueSettingsR leagueId LeagueDraftSettingsR
    case result of
        FormSuccess draftSettings -> do
            runDB $ case map entityKey maybeDraftSettings of
                Just draftSettingsId -> replace draftSettingsId draftSettings
                Nothing -> insert_ draftSettings
            randomizeDraftOrderIfRelevant userId RandomNow draftSettings seasonEntity
            setMessage "Successfully updated league draft settings"
            redirect action
        _ -> leagueSettingsLayout leagueId action enctype widget "Draft"


-------------
-- Helpers --
-------------
draftTypeWidget :: DraftType -> Widget
draftTypeWidget draftType = $(widgetFile "league/draft_type")

draftSettingsListGroupItem :: Maybe DraftSettings -> DraftType -> Widget
draftSettingsListGroupItem (Just draftSettings) draftType
    | draftSettingsDraftType draftSettings == draftType =
        [whamlet|<div .list-group-item .active>^{draftTypeWidget draftType}|]
    | otherwise = [whamlet|<div .list-group-item>^{draftTypeWidget draftType}|]
draftSettingsListGroupItem Nothing draftType
    | isDisabledDraftType draftType =
        [whamlet|<div .list-group-item .disabled>^{draftTypeWidget draftType}|]
    | otherwise = [whamlet|<a .list-group-item href="#">^{draftTypeWidget draftType}|]

