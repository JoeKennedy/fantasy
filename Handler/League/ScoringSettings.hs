module Handler.League.ScoringSettings where

import Import
import Handler.League.Setup
import Handler.League.Layout

import Data.List ((!!))

----------
-- Form --
----------
scoringSettingsForm :: UserId -> League -> [ScoringSettings] -> Html -> MForm Handler (FormResult [ScoringSettings], Widget)
scoringSettingsForm currentUserId league scoringSettingsList extra = do
    let scoringType = leagueScoringType league
    forms <- do
        isUsed <- for scoringSettingsList (\scoringSettings ->
            mreq checkBoxField "" (Just $ scoringSettingsIsUsed scoringSettings))
        pointsAndWeights <- forM scoringSettingsList (\scoringSettings -> sequence
            [ mreq intField inputRight (Just $ scoringSettingsPoints scoringSettings)
            , mreq intField inputRight (Just $ scoringSettingsWeight scoringSettings)
            , mreq intField inputRight (Just $ scoringSettingsPointsReceiving scoringSettings)
            , mreq intField inputRight (Just $ scoringSettingsWeightReceiving scoringSettings)
            ])
        return $ zip3 scoringSettingsList isUsed pointsAndWeights

    let (multiCharacterSettings, singleCharacterSettings) =
            partition (\(ss, _, _) -> isMultiCharacter $ scoringSettingsAction ss) forms
    now <- liftIO getCurrentTime

    let scoringSettingsResults =
            for forms (\(scoringSettings, isUsed, pointsAndWeights) ->
                ScoringSettings
                    <$> pure (scoringSettingsLeagueId scoringSettings)
                    <*> pure (scoringSettingsAction scoringSettings)
                    <*> fst isUsed                    -- isUsedRes
                    <*> fst ((pointsAndWeights) !! 0) -- pointsRes
                    <*> weightValue    scoringSettings scoringType (fst (pointsAndWeights !! 1))
                    <*> pointsRecValue scoringSettings scoringType (fst (pointsAndWeights !! 2))
                    <*> weightRecValue scoringSettings scoringType (fst (pointsAndWeights !! 3))
                    <*> pure (scoringSettingsCreatedBy scoringSettings)
                    <*> pure (scoringSettingsCreatedAt scoringSettings)
                    <*> updatedByField currentUserId
                    <*> pure now)

    let widget = case leagueScoringType league of Weighted -> $(widgetFile "league/weighted_scoring_settings_form")
                                                  Vanilla  -> $(widgetFile "league/vanilla_scoring_settings_form")
                                                  _        -> error "This scoring type does not use this form"
    return (scoringSettingsResults, widget)

------------
-- Routes --
------------
getSetupScoringSettingsR :: Handler Html
getSetupScoringSettingsR = do
    userId <- requireAuthId
    let action = SetupLeagueR SetupScoringSettingsR
    (Entity leagueId league, lastCompletedStep) <- leagueOrRedirect userId action
    scoringSettingsList <- runDB $ selectList [ScoringSettingsLeagueId ==. leagueId] [Asc ScoringSettingsId]
    (widget, enctype) <- generateFormPost $ scoringSettingsForm userId league $ map extractValue scoringSettingsList
    defaultLayout $ do
        setTitle $ leagueSetupStepTitle league action
        $(widgetFile "layouts/league-setup-layout")

postSetupScoringSettingsR :: Handler Html
postSetupScoringSettingsR = do
    userId <- requireAuthId
    let action = SetupLeagueR SetupScoringSettingsR
    (Entity leagueId league, lastCompletedStep) <- leagueOrRedirect userId action
    scoringSettingsList <- runDB $ selectList [ScoringSettingsLeagueId ==. leagueId] [Asc ScoringSettingsId]
    ((result, widget), enctype) <- runFormPost $ scoringSettingsForm userId league $ map extractValue scoringSettingsList
    case result of
        FormSuccess scoringSettingsList' -> do
            forM_ (zip scoringSettingsList scoringSettingsList') (\(Entity scoringSettingsId _, scoringSettings') ->
                runDB $ replace scoringSettingsId scoringSettings')
            updateLeagueLastCompletedStep leagueId league 3
            redirect $ SetupLeagueR SetupDraftSettingsR
        _ -> defaultLayout $ do
            setTitle $ leagueSetupStepTitle league action
            $(widgetFile "layouts/league-setup-layout")

getLeagueScoringSettingsR :: LeagueId -> Handler Html
getLeagueScoringSettingsR leagueId = do
    userId <- requireAuthId
    league <- runDB $ get404 leagueId
    scoringSettingsList <- runDB $ selectList [ScoringSettingsLeagueId ==. leagueId] [Asc ScoringSettingsId]
    (widget, enctype) <- generateFormPost $ scoringSettingsForm userId league $ map extractValue scoringSettingsList
    let action = LeagueSettingsR leagueId LeagueScoringSettingsR
    leagueSettingsLayout leagueId action enctype widget "Scoring"

postLeagueScoringSettingsR :: LeagueId -> Handler Html
postLeagueScoringSettingsR leagueId = do
    userId <- requireAuthId
    league <- runDB $ get404 leagueId
    scoringSettingsList <- runDB $ selectList [ScoringSettingsLeagueId ==. leagueId] [Asc ScoringSettingsId]
    ((result, widget), enctype) <- runFormPost $ scoringSettingsForm userId league $ map extractValue scoringSettingsList
    let action = LeagueSettingsR leagueId LeagueScoringSettingsR
    case result of
        FormSuccess scoringSettingsList' -> do
            forM_ (zip scoringSettingsList scoringSettingsList') (\(Entity scoringSettingsId _, scoringSettings') ->
                runDB $ replace scoringSettingsId scoringSettings')
            setMessage "Successfully updated league scoring settings"
            redirect action
        _ -> leagueSettingsLayout leagueId action enctype widget "Scoring"

-------------
-- Helpers --
-------------
scoringSettingsNotUsedError :: a
scoringSettingsNotUsedError = error "This scoring type doesn't use the scoring settings table"

weightValue :: Applicative f => ScoringSettings -> ScoringType -> f Int -> f Int
weightValue scoringSettings scoringType field =
    case scoringType of Vanilla     -> pure $ scoringSettingsWeight scoringSettings
                        Weighted    -> field
                        Scorekeeper -> scoringSettingsNotUsedError

pointsRecValue :: Applicative f => ScoringSettings -> ScoringType -> f Int -> f Int
pointsRecValue scoringSettings scoringType field =
    case (scoringType, isMultiCharacter $ scoringSettingsAction scoringSettings)
        of (Scorekeeper, _)     -> scoringSettingsNotUsedError
           (_,           True)  -> field
           (_,           False) -> pure $ scoringSettingsPointsReceiving scoringSettings

weightRecValue :: Applicative f => ScoringSettings -> ScoringType -> f Int -> f Int
weightRecValue scoringSettings scoringType field =
    case (scoringType, isMultiCharacter $ scoringSettingsAction scoringSettings)
        of (Scorekeeper, _)    -> scoringSettingsNotUsedError
           (Weighted,    True) -> field
           (_,           _)    -> pure $ scoringSettingsWeightReceiving scoringSettings


