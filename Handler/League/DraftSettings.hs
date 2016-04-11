module Handler.League.DraftSettings where

import Import
import Handler.Common        (extractValueMaybe)
import Handler.League.Setup
import Handler.League.Layout

----------
-- Form --
----------
draftSettingsForm :: UserId -> LeagueId -> Maybe DraftSettings -> Form DraftSettings
draftSettingsForm currentUserId leagueId draftSettings extra = do
    (draftTypeRes, draftTypeView) <- mreq hiddenField (hidden "Draft type")
        (draftSettingsDraftType <$> draftSettings)
    (draftOrderRes, draftOrderView) <- mreq (selectFieldList draftOrderOptions)
        (fieldName "Draft Order") (draftSettingsDraftOrder <$> draftSettings)
    (draftOrderTypeRes, draftOrderTypeView) <- mreq (selectFieldList draftOrderTypeOptions)
        (fieldName "Determination Of Draft Order") (draftSettingsDraftOrderType <$> draftSettings)
    (dateRes, dateView) <- mreq dayField (fieldName "Draft Day")
        (draftSettingsDate <$> draftSettings)
    (timeRes, timeView) <- mreq timeFieldTypeTime (fieldName "Draft Time")
        (draftSettingsTime <$> draftSettings)
    (locationRes, locationView) <- mreq textField (fieldName "Location")
        (draftSettingsLocation <$> draftSettings)
    (allowDraftPickTradingRes, allowDraftPickTradingView) <- mreq checkBoxField
        "Allow draft pick trading?" (draftSettingsAllowDraftPickTrading <$> draftSettings)
    (secondsPerPickRes, secondsPerPickView) <- mreq
        (selectFieldList $ toOptions possibleSecondsPerPick) (fieldName "Seconds Per Pick")
        (existingElseDefault defaultSecondsPerPick $ draftSettingsSecondsPerPick <$> draftSettings)
    (noteRes, noteView) <- mreq textareaField (fieldName "Note")
        (draftSettingsNote <$> draftSettings)

    now <- liftIO getCurrentTime
    let draftSettingsResult = DraftSettings
            <$> pure leagueId
            <*> draftTypeRes
            <*> draftOrderRes
            <*> draftOrderTypeRes
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
    maybeDraftSettings <- runDB $ getBy $ UniqueDraftSettingsLeagueId leagueId
    (widget, enctype) <- generateFormPost $ draftSettingsForm userId leagueId $ extractValueMaybe maybeDraftSettings
    defaultLayout $ do
        setTitle $ leagueSetupStepTitle league action
        let maybeLeagueId = Just leagueId
        $(widgetFile "layouts/league-setup-layout")

postSetupDraftSettingsR :: Handler Html
postSetupDraftSettingsR = do
    userId <- requireAuthId
    let action = SetupLeagueR SetupDraftSettingsR
    (Entity leagueId league, lastCompletedStep) <- leagueOrRedirect userId action
    maybeDraftSettings <- runDB $ getBy $ UniqueDraftSettingsLeagueId leagueId
    ((result, widget), enctype) <- runFormPost $ draftSettingsForm userId leagueId $ extractValueMaybe maybeDraftSettings
    case result of
        FormSuccess draftSettings -> do
            case maybeDraftSettings of Just (Entity dsId _) -> runDB $ replace dsId draftSettings
                                       Nothing              -> runDB $ insert_ draftSettings
            updateLeagueLastCompletedStep leagueId league 4
            redirect $ SetupLeagueR SetupTeamsSettingsR
        _ -> defaultLayout $ do
            setTitle $ leagueSetupStepTitle league action
            let maybeLeagueId = Just leagueId
            $(widgetFile "layouts/league-setup-layout")

getLeagueDraftSettingsR :: LeagueId -> Handler Html
getLeagueDraftSettingsR leagueId = do
    userId <- requireAuthId
    draftSettings <- runDB $ getBy404 $ UniqueDraftSettingsLeagueId leagueId
    (widget, enctype) <- generateFormPost $ draftSettingsForm userId leagueId $ extractValueMaybe $ Just draftSettings
    let action = LeagueSettingsR leagueId LeagueDraftSettingsR
    leagueSettingsLayout leagueId action enctype widget "Draft"

postLeagueDraftSettingsR :: LeagueId -> Handler Html
postLeagueDraftSettingsR leagueId = do
    userId <- requireAuthId
    Entity draftSettingsId draftSettings <- runDB $ getBy404 $ UniqueDraftSettingsLeagueId leagueId
    ((result, widget), enctype) <- runFormPost $ draftSettingsForm userId leagueId $ Just draftSettings
    let action = LeagueSettingsR leagueId LeagueDraftSettingsR
    case result of
        FormSuccess draftSettings' -> do
            runDB $ replace draftSettingsId draftSettings'
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
        [whamlet|<a .list-group-item .disabled href="#">^{draftTypeWidget draftType}|]
    | otherwise = [whamlet|<a .list-group-item href="#">^{draftTypeWidget draftType}|]

