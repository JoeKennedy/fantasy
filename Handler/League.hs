module Handler.League where

import Import
import Handler.Common        (extractKeyMaybe, extractValueMaybe)
import Handler.League.Setup
import Handler.League.Layout

import Data.Random.List
import Data.Random.RVar
import Data.Random.Source.DevRandom
import Network.Mail.Mime
import System.Random                (newStdGen)

----------
-- Form --
----------
leagueForm :: UserId -> Maybe League -> Form League
leagueForm currentUserId league extra = do
    (nameRes, nameView) <- mreq textField (fieldName "Name") (leagueName <$> league)
    (isPrivateRes, isPrivateView) <- mreq checkBoxField "Is league private?"
        (leagueIsPrivate <$> league)
    (scoringTypeRes, scoringTypeView) <- mreq hiddenField (hidden "Scoring type")
        (leagueScoringType <$> league)
    let teamsCount = leagueTeamsCount <$> league
    (teamsCountRes, teamsCountView) <- mreq (selectFieldList $ teamsCountOptions teamsCount)
        (fieldName "Number of teams") teamsCount

    now <- liftIO getCurrentTime
    let leagueResult = League
            <$> nameRes
            <*> existingElseDefault True (leagueIsActive <$> league)
            <*> isPrivateRes
            <*> scoringTypeRes
            <*> teamsCountRes
            <*> existingElseDefault False (leagueIsSetupComplete <$> league)
            <*> existingElseDefault 1 (leagueLastCompletedStep <$> league)
            <*> existingElseDefault False (leagueIsDraftComplete <$> league)
            <*> createdByField currentUserId (leagueCreatedBy <$> league)
            <*> existingElseDefault now (leagueCreatedAt <$> league)
            <*> updatedByField currentUserId
            <*> pure now
            <*> existingElseDefault Nothing (leagueDraftCompletedAt <$> league)
    return (leagueResult, $(widgetFile "league/league_info_form"))


------------
-- Routes --
------------
getLeaguesR :: Handler Html
getLeaguesR = do
    leagues <- runDB $ selectList [LeagueIsPrivate ==. False, LeagueIsSetupComplete ==. True] [Asc LeagueName]
    defaultLayout $ do
        setTitle "Leagues"
        $(widgetFile "league/leagues")

getLeagueR :: LeagueId -> Handler Html
getLeagueR leagueId = do
    league <- runDB $ get404 leagueId
    teams <- runDB $ selectList [TeamLeagueId ==. leagueId] [Asc TeamId]
    leagueLayout leagueId "League" $ do
        let maybeCreatorTeam = listToMaybe teams
        $(widgetFile "league/league")

postLeagueCancelR :: LeagueId -> Handler ()
postLeagueCancelR leagueId = do
    userId <- requireAuthId
    now <- liftIO getCurrentTime
    stdgen <- liftIO newStdGen
    -- Give the league a random name to avoid conflicting with new leagues
    let newLeagueName = "Canceled League " ++ toPathPiece leagueId
        randomText    = pack $ fst $ randomString 24 stdgen
    runDB $ update leagueId [ LeagueName =. newLeagueName ++ randomText
                            , LeagueIsActive =. False
                            , LeagueUpdatedBy =. userId
                            , LeagueUpdatedAt =. now
                            ]
    setMessage "Your league has been successfully canceled"

getSetupNewLeagueR :: Handler Html
getSetupNewLeagueR = do
    userId <- requireAuthId
    maybeLeague <- leagueBeingSetUp userId
    (widget, enctype) <- generateFormPost $ leagueForm userId $ extractValueMaybe maybeLeague
    defaultLayout $ do
        let title = "Create A League!" :: Html
            action = SetupLeagueR SetupNewLeagueR
            lastCompletedStep = fromMaybe 0 (leagueLastCompletedStep <$> extractValueMaybe maybeLeague)
            maybeLeagueId = extractKeyMaybe maybeLeague
        setTitle title
        $(widgetFile "layouts/league-setup-layout")

postSetupNewLeagueR :: Handler Html
postSetupNewLeagueR = do
    userId <- requireAuthId
    maybeLeague <- leagueBeingSetUp userId
    ((result, widget), enctype) <- runFormPost $ leagueForm userId $ extractValueMaybe maybeLeague
    case result of
        FormSuccess league -> do
            case maybeLeague of Just (Entity lId _) -> runDB $ replace lId league
                                Nothing             -> createLeague league
            redirect $ SetupLeagueR SetupGeneralSettingsR
        _ -> defaultLayout $ do
            let title = "Create A League!" :: Html
                action = SetupLeagueR SetupNewLeagueR
                lastCompletedStep = fromMaybe 0 (leagueLastCompletedStep <$> extractValueMaybe maybeLeague)
                maybeLeagueId = extractKeyMaybe maybeLeague
            setTitle title
            $(widgetFile "layouts/league-setup-layout")

getLeagueEditSettingsR :: LeagueId -> Handler Html
getLeagueEditSettingsR leagueId = do
    userId <- requireAuthId
    league <- runDB $ get404 leagueId
    (widget, enctype) <- generateFormPost $ leagueForm userId $ Just league
    let action = LeagueSettingsR leagueId LeagueEditSettingsR
    leagueSettingsLayout leagueId action enctype widget "League"

postLeagueEditSettingsR :: LeagueId -> Handler Html
postLeagueEditSettingsR leagueId = do
    userId <- requireAuthId
    league <- runDB $ get404 leagueId
    ((result, widget), enctype) <- runFormPost $ leagueForm userId $ Just league
    let action = LeagueSettingsR leagueId LeagueEditSettingsR
    case result of
        FormSuccess league' -> do
            runDB $ replace leagueId league'
            setMessage "Successfully updated league settings"
            redirect action
        _ -> leagueSettingsLayout leagueId action enctype widget "League"

-------------------
-- Create League --
-------------------
createLeague :: League -> Handler ()
createLeague league = do
    let teamNumbers = [1..(leagueTeamsCount league)]
    draftOrder <- liftIO (runRVar (shuffle teamNumbers) DevRandom :: IO [Int])
    runDB $ do
        leagueId <- insert league
        let teamsCount = leagueTeamsCount league
            leagueEntity = Entity leagueId league
        insert_ $ GeneralSettings
            { generalSettingsLeagueId = leagueId
            , generalSettingsNumberOfStarters = fst $ defaultRosterSize teamsCount
            , generalSettingsRosterSize = snd $ defaultRosterSize teamsCount
            , generalSettingsRegularSeasonLength = fst $ defaultSeasonLength teamsCount
            , generalSettingsPlayoffLength = snd $ defaultSeasonLength teamsCount
            , generalSettingsNumberOfTeamsInPlayoffs = defaultNumberOfTeamsInPlayoffs teamsCount
            , generalSettingsTradeDeadlineWeek = defaultTradeDeadlineWeek teamsCount
            , generalSettingsWaiverPeriodInDays = defaultWaiverPeriodInDays
            , generalSettingsCreatedBy = leagueCreatedBy league
            , generalSettingsCreatedAt = leagueCreatedAt league
            , generalSettingsUpdatedBy = leagueUpdatedBy league
            , generalSettingsUpdatedAt = leagueUpdatedAt league
            }
        mapM_ (createScoringSettingsRow leagueEntity) allActions
        mapM_ (createTeam leagueEntity) $ zip teamNumbers draftOrder
        characters <- selectList [] [Asc CharacterName]
        mapM_ (createPlayer leagueEntity) characters
        return ()

createScoringSettingsRow :: Entity League -> Action -> ReaderT SqlBackend Handler ()
createScoringSettingsRow (Entity leagueId league) action =
    let (isUsed, points, weight, pointsRec, weightRec) =
            defaultScoringAttributes action $ leagueScoringType league
    in  insert_ $ ScoringSettings
            { scoringSettingsLeagueId = leagueId
            , scoringSettingsAction = action
            , scoringSettingsIsUsed = isUsed
            , scoringSettingsPoints = points
            , scoringSettingsWeight = weight
            , scoringSettingsPointsReceiving = pointsRec
            , scoringSettingsWeightReceiving = weightRec
            , scoringSettingsCreatedBy = leagueCreatedBy league
            , scoringSettingsCreatedAt = leagueCreatedAt league
            , scoringSettingsUpdatedBy = leagueUpdatedBy league
            , scoringSettingsUpdatedAt = leagueUpdatedAt league
            }

createTeam :: Entity League -> (Int, Int) -> ReaderT SqlBackend Handler ()
createTeam (Entity leagueId league) (teamNumber, draftOrder) = do
    stdgen <- liftIO newStdGen
    let (name, abbrev, owner, email) = teamTextAttributes teamNumber
        (maybeTeamOwnerId, maybeConfirmedAt) = teamNonTextAttributes league teamNumber
        verificationKey = pack $ fst $ randomString 24 stdgen
    insert_ $ Team { teamLeagueId         = leagueId
                   , teamName             = name
                   , teamAbbreviation     = abbrev
                   , teamOwnerId          = maybeTeamOwnerId
                   , teamOwnerName        = owner
                   , teamOwnerEmail       = email
                   , teamIsConfirmed      = isJust maybeConfirmedAt
                   , teamPlayersCount     = 0
                   , teamStartersCount    = 0
                   , teamDraftOrder       = draftOrder
                   , teamWaiverOrder      = teamNumber
                   , teamVerificationKey  = verificationKey
                   , teamPointsThisSeason = 0
                   , teamCreatedBy        = leagueCreatedBy league
                   , teamCreatedAt        = leagueCreatedAt league
                   , teamUpdatedBy        = leagueUpdatedBy league
                   , teamUpdatedAt        = leagueUpdatedAt league
                   , teamConfirmedBy      = maybeTeamOwnerId
                   , teamConfirmedAt      = maybeConfirmedAt
                   }

teamTextAttributes :: Int -> (Text, Text, Text, Text)
teamTextAttributes 1 = ("My House Name", "N1", "My Name", "My Email Address")
teamTextAttributes n =
    let s = (pack . show) n
    in  ("Number " ++ s, "N" ++ s, "Owner " ++ s, "Owner " ++ s ++ "'s email")

teamNonTextAttributes :: League -> Int -> (Maybe UserId, Maybe UTCTime)
teamNonTextAttributes league 1 =
    (Just $ leagueCreatedBy league, Just $ leagueCreatedAt league)
teamNonTextAttributes _ _ = (Nothing, Nothing)

createPlayer :: Entity League -> Entity Character -> ReaderT SqlBackend Handler ()
createPlayer (Entity leagueId league) (Entity characterId character) =
    insert_ $ Player { playerLeagueId         = leagueId
                     , playerCharacterId      = characterId
                     , playerTeamId           = Nothing
                     , playerIsStarter        = False
                     , playerPointsThisSeason = 0
                     , playerIsPlayable       = characterIsPlayable character
                     , playerCreatedBy        = leagueCreatedBy league
                     , playerCreatedAt        = leagueCreatedAt league
                     , playerUpdatedBy        = leagueUpdatedBy league
                     , playerUpdatedAt        = leagueUpdatedAt league
                     }

-------------
-- Helpers --
-------------
scoringTypeWidget :: ScoringType -> Widget
scoringTypeWidget scoringType = $(widgetFile "league/scoring_type")

leagueListGroupItem :: Maybe League -> ScoringType -> Widget
leagueListGroupItem (Just league) scoringType
    | leagueScoringType league == scoringType =
        [whamlet|<div .list-group-item .active>^{scoringTypeWidget scoringType}|]
    | otherwise = [whamlet|<div .list-group-item>^{scoringTypeWidget scoringType}|]
leagueListGroupItem Nothing scoringType
    | isDisabledScoringType scoringType =
        [whamlet|<div .list-group-item .disabled>^{scoringTypeWidget scoringType}|]
    | otherwise = [whamlet|<a .list-group-item href="#">^{scoringTypeWidget scoringType}|]

