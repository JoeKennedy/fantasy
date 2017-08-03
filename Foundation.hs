module Foundation where

import Import.NoFoundation

import qualified Database.Esqueleto as E
import           Database.Esqueleto ((^.))
import           Database.Persist.Sql                 (ConnectionPool, runSqlPool)
import           Data.Maybe                           (fromJust)
import           Data.UUID                            (UUID)
import           Facebook                             (Credentials(..))
import           Network.Mail.Mime.SES
import           Text.Hamlet                          (hamletFile)
import           Text.Jasmine                         (minifym)
import           Web.ServerSession.Backend.Persistent
import           Web.ServerSession.Frontend.Yesod
import qualified Yesod.Auth.GoogleEmail2        as GE
import qualified Yesod.Auth.Facebook.ServerSide as FB
import           Yesod.Auth.OpenId
import           Yesod.Core.Types                     (Logger)
import qualified Yesod.Core.Unsafe              as US
import           Yesod.Default.Util                   (addStaticContentExternal)
import qualified Yesod.Facebook                 as YF

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings           :: AppSettings
    , appStatic             :: Static -- ^ Settings for static file serving.
    , appConnPool           :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager        :: Manager
    , appLogger             :: Logger
    , appFacebookOAuth2Keys :: OAuth2Keys
    , appGoogleOAuth2Keys   :: OAuth2Keys
    , appSesCreds           :: Text -> SES
    , appAcmeChallenge      :: Text
    , appLetsEncrypt        :: Text
    }

instance HasHttpManager App where
    getHttpManager = appHttpManager

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the linked documentation for an
-- explanation for this split.
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerT App IO
-- type Widget = WidgetT App IO ()
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot = ApprootMaster $ appRoot . appSettings

    -- Store session data on the server with default settings
    makeSessionBackend = simpleBackend id . SqlStorage . appConnPool

    defaultLayout widget = do
        master <- getYesod
        maybeUser <- maybeAuth
        messages <- getMessages
        (title', parents) <- breadcrumbs

        seriesList <- runDB $ selectList [] [Asc SeriesNumber]
        setUltDestCurrent

        leagues <- getLeaguesByUser $ map entityKey maybeUser

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            addStylesheet $ StaticR css_bootstrap_css
            addStylesheet $ StaticR css_font_awesome_css
            addStylesheet $ StaticR css_bootstrap_social_css
            addStylesheet $ StaticR css_bootstrap_table_css
            addStylesheet $ StaticR css_bootstrap_override_css
            addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/2.1.0/jquery.min.js"
            addScript $ StaticR js_bootstrap_js
            addScript $ StaticR js_bootstrap_table_js
            addScript $ StaticR js_bootstrap_override_js
            $(widgetFile "layouts/default-layout")
        withUrlRenderer $(hamletFile "templates/layouts/default-layout-wrapper.hamlet")

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- determine authorization for routes
    isAuthorized (AuthR _)   _ = return Authorized
    isAuthorized FaviconR    _ = return Authorized
    isAuthorized RobotsR     _ = return Authorized
    isAuthorized (StaticR _) _ = return Authorized
    isAuthorized HomeR       _ = return Authorized
    isAuthorized FAQR        _ = return Authorized
    isAuthorized LetsEncryptR{} _ = return Authorized

    isAuthorized (AdminR _) _ = requireAdmin

    isAuthorized CharactersR          _ = return Authorized
    isAuthorized (CharacterR _)       _ = return Authorized
    isAuthorized HousesR              _ = return Authorized
    isAuthorized (HouseR _)           _ = return Authorized
    isAuthorized SpeciesListR         _ = return Authorized
    isAuthorized (SpeciesR _)         _ = return Authorized
    isAuthorized SeriesListR          _ = return Authorized
    isAuthorized (SeriesR _)          _ = return Authorized
    isAuthorized (SeriesEpisodesR _)  _ = return Authorized
    isAuthorized (SeriesEpisodeR _ _) _ = return Authorized

    isAuthorized LeaguesR                          _ = return Authorized
    isAuthorized (LeagueR leagueId)                _ = requirePublicOrLeagueMember leagueId
    isAuthorized (LeagueCancelR leagueId)          _ = requireLeagueManager leagueId
    isAuthorized (LeagueSeasonR leagueId year)     _ = requireSeasonAccessible leagueId year
    isAuthorized (LeagueDraftR leagueId)           _ = requireLeagueManagerAndIncompleteDraft leagueId
    isAuthorized (LeagueTransactionsR leagueId)    _ = requirePublicOrLeagueMember leagueId
    isAuthorized (LeagueAcceptTradeR lid tid)      _ = requireTradeAcceptable lid tid
    isAuthorized (LeagueDeclineTradeR lid tid)     _ = requireTradeDeclinable lid tid
    isAuthorized (LeagueCancelTransactionR lid tid)_ = requireTransactionCancelable lid tid
    isAuthorized (LeagueMoveClaimUpR lid tid)      _ = requireClaimMovableUp lid tid
    isAuthorized (LeagueMoveClaimDownR lid tid)    _ = requireClaimMovableDown lid tid

    isAuthorized (LeagueTeamsR leagueId)           _ = requirePublicOrLeagueMember leagueId
    isAuthorized (LeagueTeamR leagueId _)          _ = requirePublicOrLeagueMember leagueId
    isAuthorized (LeagueTeamJoinR lid tid ver)  True = requireCorrectVerKeyAndLoggedIn lid tid ver
    isAuthorized (LeagueTeamJoinR lid tid ver) False = requireCorrectVerificationKey lid tid ver
    isAuthorized (LeagueTeamSettingsR lid tNumber) _ = requireTeamOwnerByNumber lid tNumber
    isAuthorized (LeagueTeamResendR lid tNumber)   _ = requireJoinEmailResendable lid tNumber

    isAuthorized (LeagueResultsR leagueId)         _ = requirePublicOrLeagueMember leagueId
    isAuthorized (LeaguePlayoffsR leagueId)        _ = requireSeasonInPostSeason leagueId
    isAuthorized (LeagueResultsWeekR lid weekNo)   _ = requireWeekExists lid weekNo
    isAuthorized (LeaguePlayersR leagueId)         _ = requirePublicOrLeagueMember leagueId
    isAuthorized (LeaguePlayerR leagueId characId) _ = requirePlayable leagueId characId
    isAuthorized (LeaguePlayerStartR lid characId) _ = requirePlayerStartable lid characId
    isAuthorized (LeaguePlayerBenchR lid characId) _ = requirePlayerBenchable lid characId
    isAuthorized (LeaguePlayerClaimR lid cid rcid) _ = requireClaimPossible lid cid rcid
    isAuthorized (LeaguePlayerTradeR lid cid rcid) _ = requireTradePossible lid cid rcid

    isAuthorized (LeagueSettingsR leagueId _)   True = requireLeagueManager leagueId
    isAuthorized (LeagueSettingsR leagueId _)  False = requirePublicOrLeagueMember leagueId

    isAuthorized (SetupLeagueR _) _ = requireLoggedIn

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog app _source level =
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger = return . appLogger

requireLoggedIn :: Handler AuthResult
requireLoggedIn = do
    mu <- maybeAuth
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just _  -> Authorized

requireAdmin :: Handler AuthResult
requireAdmin = do
    mu <- maybeAuth
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just (Entity _ u) ->
            if userIsAdmin u then Authorized else Unauthorized "You must be an admin"

requireAdminIfPost :: Bool -> Handler AuthResult
requireAdminIfPost True  = requireAdmin
requireAdminIfPost False = return Authorized

requireLeagueMember :: LeagueId -> Handler AuthResult
requireLeagueMember leagueId = do
    muid <- maybeAuthId
    case muid of
        Nothing  -> return AuthenticationRequired
        Just uid -> do
            teams <- runDB $ selectList [TeamLeagueId ==. leagueId] []
            case find (\(Entity _ t) -> teamOwnerId t == Just uid) teams of
                Just _  -> return Authorized
                Nothing -> return $ Unauthorized "You are not a member of this league"

requirePublicOrLeagueMember :: LeagueId -> Handler AuthResult
requirePublicOrLeagueMember leagueId = do
    league <- runDB $ get404 leagueId
    if leagueIsPrivate league
        then requireLeagueMember leagueId
        else return Authorized

requireSeasonAccessible :: LeagueId -> Int -> Handler AuthResult
requireSeasonAccessible leagueId year = do
    authResult <- requirePublicOrLeagueMember leagueId
    case authResult of
        Authorized -> do
          maybeSeasonEntity <- runDB $ getBy $ UniqueSeasonLeagueIdYear leagueId year
          case maybeSeasonEntity of Just _  -> return Authorized
                                    Nothing -> return $ Unauthorized "Season was not found"
        _ -> return authResult

requirePlayable :: LeagueId -> CharacterId -> Handler AuthResult
requirePlayable leagueId characterId = do
    Entity _ player <- runDB $ getBy404 $ UniquePlayerLeagueIdCharacterId leagueId characterId
    if playerIsPlayable player
        then requirePublicOrLeagueMember leagueId
        else return $ Unauthorized "Player is not playable"

requireSeasonInPostSeason :: LeagueId -> Handler AuthResult
requireSeasonInPostSeason leagueId = do
    publicOrLeagueMember <- requirePublicOrLeagueMember leagueId
    case publicOrLeagueMember of
        Authorized -> do
            Entity _ season <- getSelectedSeason leagueId
            return $ if seasonIsInPostSeason season
                then Authorized
                else Unauthorized "Season is not in postseason"
        _ -> return publicOrLeagueMember

requireJoinEmailResendable :: LeagueId -> Int -> Handler AuthResult
requireJoinEmailResendable leagueId number = do
    authResult <- requireLeagueManager leagueId
    case authResult of
        Authorized -> do
            league <- runDB $ get404 leagueId
            Entity _ team <- runDB $ getBy404 $ UniqueTeamLeagueIdNumber leagueId number
            emailSentRecently <- liftIO $ past24Hours $ teamJoinEmailResentAt team
            return $ if leagueIsActive league && leagueIsSetupComplete league
                then if teamIsConfirmed team
                        then Unauthorized "Team is already confirmed"
                        else if emailSentRecently
                                then Unauthorized "Join email was resent in past 24 hours"
                                else Authorized
                else Unauthorized "League must be set up and active"
        _ -> return authResult

requireWeekExists :: LeagueId -> Int -> Handler AuthResult
requireWeekExists leagueId weekNo = do
    seasonId <- getSelectedSeasonId leagueId
    maybeWeek <- runDB $ getBy $ UniqueWeekSeasonIdNumber seasonId weekNo
    case maybeWeek of Just _  -> requirePublicOrLeagueMember leagueId
                      Nothing -> return $ Unauthorized "Week does not exist"

requireTransactionInLeague :: LeagueId -> Transaction -> AuthResult
requireTransactionInLeague leagueId transaction =
    if leagueId == transactionLeagueId transaction
        then Authorized else Unauthorized "Transaction is not in this league"

requireTradeAcceptable :: LeagueId -> TransactionId -> Handler AuthResult
requireTradeAcceptable leagueId transactionId = do
    transaction <- runDB $ get404 transactionId
    let transactionInLeague = requireTransactionInLeague leagueId transaction
    if transactionInLeague /= Authorized then return transactionInLeague else do
        Entity _ season <- getSelectedSeason $ transactionLeagueId transaction
        muid <- maybeAuthId
        case (muid, transactionOtherTeamId transaction) of
            (Nothing, _)     -> return AuthenticationRequired
            (_, Nothing)     -> return $ Unauthorized "Transaction must have another team"
            (_, Just teamId) ->
                if transactionStatus transaction == Requested && transactionType transaction == Trade
                    then if seasonIsAfterTradeDeadline season
                             then return $ Unauthorized "Trades cannot happen after trade deadline"
                             else requireTeamOwner teamId
                    else return $ Unauthorized "Must be a trade transaction with status of requested"

requireTradeDeclinable :: LeagueId -> TransactionId -> Handler AuthResult
requireTradeDeclinable = requireTradeAcceptable

requireTransactionCancelable :: LeagueId -> TransactionId -> Handler AuthResult
requireTransactionCancelable leagueId transactionId = do
    transaction <- runDB $ get404 transactionId
    let transactionInLeague = requireTransactionInLeague leagueId transaction
    if transactionInLeague /= Authorized then return transactionInLeague else do
        Entity _ season <- getSelectedSeason $ transactionLeagueId transaction
        let teamId = transactionTeamId transaction
        if transactionStatus transaction == Requested
            then case transactionType transaction of
                     Claim -> requireTeamOwner teamId
                     Trade -> if seasonIsAfterTradeDeadline season
                                  then return $ Unauthorized "Must be a trade transaction with status of requested"
                                  else requireTeamOwner teamId
                     _ -> return $ Unauthorized "Must be a trade or claim transaction"
            else return $ Unauthorized "Must be a transaction with status of requested"

requireClaimMovableUp :: LeagueId -> TransactionId -> Handler AuthResult
requireClaimMovableUp leagueId transactionId = do
    transaction <- runDB $ get404 transactionId
    let transactionInLeague = requireTransactionInLeague leagueId transaction
    if transactionInLeague /= Authorized then return transactionInLeague else
        if fromMaybe 1 (transactionPosition transaction) > 1 && transactionType transaction == Claim
            then requireTransactionCancelable leagueId transactionId
            else return $ Unauthorized "Must be a claim transaction that isn't first"

requireClaimMovableDown :: LeagueId -> TransactionId -> Handler AuthResult
requireClaimMovableDown leagueId transactionId = do
    transaction <- runDB $ get404 transactionId
    let transactionInLeague = requireTransactionInLeague leagueId transaction
    if transactionInLeague /= Authorized then return transactionInLeague else do
        let teamId = transactionTeamId transaction
        claimRequests <- runDB $ count [ TransactionTeamId ==. teamId
                                       , TransactionType   ==. Claim
                                       , TransactionStatus ==. Requested
                                       ]
        if fromMaybe claimRequests (transactionPosition transaction) < claimRequests
            && transactionType transaction == Claim
                then requireTransactionCancelable leagueId transactionId
                else return $ Unauthorized "Must be a claim transaction that isn't first"

requireCorrectVerificationKey :: LeagueId -> Int -> Text -> Handler AuthResult
requireCorrectVerificationKey leagueId number verificationKey = do
    Entity _ team <- runDB $ getBy404 $ UniqueTeamLeagueIdNumber leagueId number
    return $ if teamVerificationKey team == verificationKey
        then Authorized
        else Unauthorized "You aren't allowed to join this league"

requireCorrectVerKeyAndLoggedIn :: LeagueId -> Int -> Text -> Handler AuthResult
requireCorrectVerKeyAndLoggedIn leagueId number verKey = do
    authResult <- requireLoggedIn
    case authResult of Authorized -> requireCorrectVerificationKey leagueId number verKey
                       _ -> return authResult

requireTeamOwnerByNumber :: LeagueId -> Int -> Handler AuthResult
requireTeamOwnerByNumber leagueId number = do
    Entity teamId _ <- runDB $ getBy404 $ UniqueTeamLeagueIdNumber leagueId number
    requireTeamOwner teamId

requireTeamOwner :: TeamId -> Handler AuthResult
requireTeamOwner teamId = do
    team <- runDB $ get404 teamId
    muid <- maybeAuthId
    let unauthorized = Unauthorized "You are not the owner of this team"
    return $ case muid of
        Nothing -> AuthenticationRequired
        _ -> if teamOwnerId team == muid then Authorized else unauthorized

-- TODO - this function will need to change later so that someone
-- other than the creator can manage the league
-- In addition, this should be used when determining
-- whether or not users can see fields for the settings
-- Easiest way to start might be to just disable all fields
-- if the user can't manage the league
requireLeagueManager :: LeagueId -> Handler AuthResult
requireLeagueManager leagueId = do
    league <- runDB $ get404 leagueId
    muid <- maybeAuthId
    return $ case muid of
        Nothing -> AuthenticationRequired
        Just uid ->
            if uid == leagueCreatedBy league
                then Authorized
                else Unauthorized "You aren't allowed to manage this league"

requireLeagueManagerAndIncompleteDraft :: LeagueId -> Handler AuthResult
requireLeagueManagerAndIncompleteDraft leagueId = do
    Entity seasonId season <- getSelectedSeason leagueId
    authResult <- requireLeagueManager leagueId
    case authResult of
        Authorized -> if seasonIsDraftComplete season
            then return $ Unauthorized "Draft already completed"
            else do
                maybeDraftSettings <- runDB $ getBy $ UniqueDraftSettingsSeasonId seasonId
                return $ case maybeDraftSettings of
                    Just _ -> Authorized
                    Nothing -> Unauthorized "Draft settings must be filled out for this season"
        _ -> return $ authResult

requireSinglePlayerTransactionPossible :: Bool -> LeagueId -> CharacterId -> Handler AuthResult
requireSinglePlayerTransactionPossible isStarter leagueId characterId = do
    result <- requirePlayerOwnerAndTransactionsPossible leagueId characterId
    case result of
        Right authResult -> return authResult
        Left playerSeason ->
            case (playerSeasonIsStarter playerSeason, isStarter) of
                (True,  True)  -> return $ Authorized
                (True,  False) -> return $ Unauthorized "Player must be on the bench"
                (False, True)  -> return $ Unauthorized "Player must be starting"
                (False, False) -> do
                    let seasonId = playerSeasonSeasonId playerSeason
                        teamId = fromJust $ playerSeasonTeamId playerSeason
                    Entity _ generalSettings <- runDB $ getBy404 $ UniqueGeneralSettingsSeasonId seasonId
                    Entity _ teamSeason <- runDB $ getBy404 $ UniqueTeamSeasonTeamIdSeasonId teamId seasonId
                    return $ if teamSeasonStartersCount teamSeason < generalSettingsNumberOfStarters generalSettings
                        then Authorized
                        else Unauthorized "Another player must be benched before this one can be started"

requireMultiPlayerTransactionPossible :: Bool -> LeagueId -> CharacterId -> CharacterId -> Handler AuthResult
requireMultiPlayerTransactionPossible onTeam leagueId yourCharacterId myCharacterId = do
    result <- requirePlayerOwnerAndTransactionsPossible leagueId myCharacterId
    case result of
        Right authResult -> return authResult
        Left myPlayerSeason -> do
            Entity yourPlayerId _ <- runDB $ getBy404 $ UniquePlayerLeagueIdCharacterId leagueId yourCharacterId
            seasonId <- getSelectedSeasonId leagueId
            Entity _ yourPlayerSeason <- runDB $ getBy404 $ UniquePlayerSeasonPlayerIdSeasonId yourPlayerId seasonId
            let string = if onTeam then "on a team" else "a free agent"
            if playerSeasonTeamId myPlayerSeason == playerSeasonTeamId yourPlayerSeason
                then return $ Unauthorized "Players must not be on the same team"
                else return $ if isJust (playerSeasonTeamId yourPlayerSeason) == onTeam
                    then Authorized else Unauthorized $ "Player must be " ++ string

requirePlayerStartable :: LeagueId -> CharacterId -> Handler AuthResult
requirePlayerStartable = requireSinglePlayerTransactionPossible False

requirePlayerBenchable :: LeagueId -> CharacterId -> Handler AuthResult
requirePlayerBenchable = requireSinglePlayerTransactionPossible True

requireClaimPossible :: LeagueId -> CharacterId -> CharacterId -> Handler AuthResult
requireClaimPossible = requireMultiPlayerTransactionPossible False

requireTradePossible :: LeagueId -> CharacterId -> CharacterId -> Handler AuthResult
requireTradePossible = requireMultiPlayerTransactionPossible True

requirePlayerOwnerAndTransactionsPossible :: LeagueId -> CharacterId -> Handler (Either PlayerSeason AuthResult)
requirePlayerOwnerAndTransactionsPossible leagueId characterId = do
    Entity _ season <- getSelectedSeason leagueId
    muid <- maybeAuthId
    case (muid, seasonIsDraftComplete season, seasonIsSeasonComplete season) of
        (Nothing, _, _) -> return $ Right AuthenticationRequired
        (_, False, _)   -> return $ Right $ Unauthorized "Transactions cannot happen before draft occurs"
        (_, _, True)    -> return $ Right $ Unauthorized "Transactions cannot happen after season is complete"
        (_, _, _)       -> requirePlayerOwner leagueId characterId

requirePlayerOwner :: LeagueId -> CharacterId -> Handler (Either PlayerSeason AuthResult)
requirePlayerOwner leagueId characterId = do
    Entity playerId _ <- runDB $ getBy404 $ UniquePlayerLeagueIdCharacterId leagueId characterId
    seasonId <- getSelectedSeasonId leagueId
    Entity _ playerSeason <- runDB $ getBy404 $ UniquePlayerSeasonPlayerIdSeasonId playerId seasonId
    let unauthorized = Right $ Unauthorized "This player is not on your team"
    case playerSeasonTeamId playerSeason of
        Nothing  -> return unauthorized
        Just tid -> do
            team <- runDB $ get404 tid
            muid <- maybeAuthId
            return $ if teamOwnerId team == muid then Left playerSeason else unauthorized


instance YesodBreadcrumbs App where
    breadcrumb HomeR = return ("Home", Nothing)
    breadcrumb (AuthR LoginR) = return ("Sign In", Just HomeR)
    breadcrumb FAQR = return ("How To Play", Just HomeR)

    -- Admin
    breadcrumb (AdminR AdminDashboardR) = return ("Dashboard", Nothing)
    breadcrumb (AdminR AdminScoreR) = return ("Score", Just $ AdminR AdminDashboardR)
    breadcrumb (AdminR (AdminScoreEpisodeR eid)) = return (toPathPiece eid, Just $ AdminR AdminScoreR)

    breadcrumb (AdminR AdminBlurbsR)      = return ("Blurbs", Just $ AdminR AdminDashboardR)
    breadcrumb (AdminR (AdminBlurbR bid)) = return (toPathPiece bid, Just $ AdminR AdminBlurbsR)

    breadcrumb (AdminR AdminCharactersR)      = return ("Characters", Just $ AdminR AdminDashboardR)
    breadcrumb (AdminR (AdminCharacterR cid)) = return (toPathPiece cid, Just $ AdminR AdminCharactersR)

    breadcrumb (AdminR AdminEpisodesR)      = return ("Episodes", Just $ AdminR AdminDashboardR)
    breadcrumb (AdminR (AdminEpisodeR hid)) = return (toPathPiece hid, Just $ AdminR AdminEpisodesR)

    breadcrumb (AdminR AdminEventsR)      = return ("Events", Just $ AdminR AdminDashboardR)
    breadcrumb (AdminR (AdminEventR hid)) = return (toPathPiece hid, Just $ AdminR AdminEventsR)

    breadcrumb (AdminR AdminHousesR)      = return ("Houses", Just $ AdminR AdminDashboardR)
    breadcrumb (AdminR (AdminHouseR hid)) = return (toPathPiece hid, Just $ AdminR AdminHousesR)

    breadcrumb (AdminR AdminSeriesListR)   = return ("Series", Just $ AdminR AdminDashboardR)
    breadcrumb (AdminR (AdminSeriesR sid)) = return (toPathPiece sid, Just $ AdminR AdminSeriesListR)

    breadcrumb (AdminR AdminSpeciesListR)   = return ("Species", Just $ AdminR AdminDashboardR)
    breadcrumb (AdminR (AdminSpeciesR sid)) = return (toPathPiece sid, Just $ AdminR AdminSpeciesListR)

    -- Character, species, and house breadcrumbs
    breadcrumb CharactersR = return ("Characters", Just HomeR)
    breadcrumb (CharacterR characterId) = do
        character <- runDB $ get404 characterId
        return (characterName character, Just CharactersR)

    breadcrumb SpeciesListR = return ("Species", Just HomeR)
    breadcrumb (SpeciesR speciesId) = do
        species <- runDB $ get404 speciesId
        return (speciesName species, Just SpeciesListR)

    breadcrumb HousesR = return ("Houses", Just HomeR)
    breadcrumb (HouseR houseId) = do
        house <- runDB $ get404 houseId
        return (houseName house, Just HousesR)

    -- Series breadcrumbs
    breadcrumb SeriesListR = return ("Seasons", Just HomeR)
    breadcrumb (SeriesR seriesNo) = return (toPathPiece seriesNo, Just SeriesListR)
    breadcrumb (SeriesEpisodesR seriesNo) = return ("Episodes", Just $ SeriesR seriesNo)
    breadcrumb (SeriesEpisodeR seriesNo episodeNo) =
        return (toPathPiece episodeNo, Just $ SeriesEpisodesR seriesNo)

    -- League breadcrumbs
    breadcrumb LeaguesR           = return ("Leagues", Just HomeR)
    breadcrumb (LeagueR leagueId) = do
        league <- runDB $ get404 leagueId
        return (leagueName league, Just LeaguesR)
    breadcrumb (LeagueCancelR leagueId)         = return ("Cancel", Just $ LeagueR leagueId)
    breadcrumb (LeagueDraftR leagueId)          = return ("Draft", Just $ LeagueR leagueId)
    breadcrumb (LeagueTransactionsR leagueId)   = return ("Transactions", Just $ LeagueR leagueId)
    breadcrumb (LeagueAcceptTradeR leagueId _)  = return ("Accept Trade", Just $ LeagueTransactionsR leagueId)
    breadcrumb (LeagueDeclineTradeR leagueId _) = return ("Decline Trade", Just $ LeagueTransactionsR leagueId)

    -- League players breadcrumbs
    breadcrumb (LeaguePlayersR leagueId) = return ("Characters", Just $ LeagueR leagueId)
    breadcrumb (LeaguePlayerR leagueId characterId) = do
        character <- runDB $ get404 characterId
        return (characterName character, Just $ LeagueR leagueId)

    -- League teams breadcrumbs
    breadcrumb (LeagueTeamsR leagueId) = return ("Houses", Just $ LeagueR leagueId)
    breadcrumb (LeagueTeamR leagueId number) = do
        Entity _ team <- runDB $ getBy404 $ UniqueTeamLeagueIdNumber leagueId number
        return ("House " ++ teamName team, Just $ LeagueTeamsR leagueId)
    breadcrumb (LeagueTeamSettingsR leagueId number) = return ("Settings", Just $ LeagueTeamR leagueId number)
    breadcrumb (LeagueTeamJoinR leagueId number _)   = return ("Join", Just $ LeagueTeamR leagueId number)

    -- League week breadcrumbs
    breadcrumb (LeagueResultsR leagueId)  = return ("Results",  Just $ LeagueR leagueId)
    breadcrumb (LeaguePlayoffsR leagueId) = return ("Playoffs", Just $ LeagueR leagueId)
    breadcrumb (LeagueResultsWeekR leagueId weekNo) = return ("Week " ++ pack (show weekNo), Just $ LeagueResultsR leagueId)

    -- League settings breadcrumbs
    breadcrumb (LeagueSettingsR leagueId LeagueEditSettingsR)    = return ("League Settings", Just $ LeagueR leagueId)
    breadcrumb (LeagueSettingsR leagueId LeagueGeneralSettingsR) = return ("General Settings", Just $ LeagueR leagueId)
    breadcrumb (LeagueSettingsR leagueId LeagueScoringSettingsR) = return ("Scoring Settings", Just $ LeagueR leagueId)
    breadcrumb (LeagueSettingsR leagueId LeagueDraftSettingsR)   = return ("Draft Settings", Just $ LeagueR leagueId)
    breadcrumb (LeagueSettingsR leagueId LeagueTeamsSettingsR)   = return ("Team Settings", Just $ LeagueR leagueId)

    -- League setup breadcrumbs
    breadcrumb (SetupLeagueR SetupLeagueStartR)     = return ("Setup", Just LeaguesR)
    breadcrumb (SetupLeagueR SetupGeneralSettingsR) = return ("General Settings", Just $ SetupLeagueR SetupLeagueStartR)
    breadcrumb (SetupLeagueR SetupScoringSettingsR) = return ("Scoring Settings", Just $ SetupLeagueR SetupGeneralSettingsR)
    breadcrumb (SetupLeagueR SetupDraftSettingsR)   = return ("Draft Settings", Just $ SetupLeagueR SetupScoringSettingsR)
    breadcrumb (SetupLeagueR SetupTeamsSettingsR)   = return ("Team Settings", Just $ SetupLeagueR SetupDraftSettingsR)
    breadcrumb (SetupLeagueR SetupConfirmSettingsR) = return ("Complete Setup", Just $ SetupLeagueR SetupTeamsSettingsR)

    -- These pages never call breadcrumb
    breadcrumb StaticR{}      = return ("", Nothing)
    breadcrumb AuthR{}        = return ("", Nothing)
    breadcrumb FaviconR       = return ("", Nothing)
    breadcrumb RobotsR        = return ("", Nothing)
    breadcrumb LetsEncryptR{} = return ("", Nothing)

    breadcrumb (AdminR AdminScoreEventR{}) = return ("", Nothing)
    breadcrumb (SetupLeagueR SetupNewLeagueR) = return ("", Nothing)

    breadcrumb LeagueSeasonR{}            = return ("", Nothing)
    breadcrumb LeagueCancelTransactionR{} = return ("", Nothing)
    breadcrumb LeagueMoveClaimUpR{}       = return ("", Nothing)
    breadcrumb LeagueMoveClaimDownR{}     = return ("", Nothing)
    breadcrumb LeagueTeamResendR{}        = return ("", Nothing)

    breadcrumb LeaguePlayerStartR{}       = return ("", Nothing)
    breadcrumb LeaguePlayerBenchR{}       = return ("", Nothing)
    breadcrumb LeaguePlayerClaimR{}       = return ("", Nothing)
    breadcrumb LeaguePlayerTradeR{}       = return ("", Nothing)

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest _ = HomeR
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer _ = True

    authenticate creds = do
        maybeUser <- maybeAuth
        maybeIdent <- runDB $ do
            maybeIdentClaimed <- getBy $ UniqueIdent $ credsIdentClaimed creds
            case maybeIdentClaimed of
                Just _ -> return maybeIdentClaimed
                Nothing -> getBy $ UniqueIdent $ credsIdent creds
        case (maybeIdent, maybeUser) of
            (Just (Entity _ ident), Nothing) -> do
                runDB $ addClaimed (identUserId ident) creds
                return $ Authenticated $ identUserId ident
            (Nothing, Nothing) -> runDB $ do
                uid <- insert $ User
                    { userFirstName = Nothing
                    , userLastName  = Nothing
                    , userIsAdmin   = False
                    }
                addClaimed uid creds
                return $ Authenticated uid
            (Nothing, Just (Entity uid _)) -> do
                setMessage "Identify added to your account"
                runDB $ do
                    _ <- insert $ Ident (credsIdent creds) uid
                    addClaimed uid creds
                return $ Authenticated uid
            (Just _, Just (Entity uid _)) -> do
                runDB $ addClaimed uid creds
                setMessage "That identifier is already attached to an account. Please detach it from the other account first."
                redirect HomeR

        where
            addClaimed uid creds' = do
              let claimed = credsIdentClaimed creds'
              _ <- insertBy $ Ident claimed uid
              return ()

    -- You can add other plugins like BrowserID, email or OAuth here
    authPlugins m =
        [ GE.authGoogleEmail
            (clientId $ appGoogleOAuth2Keys m)
            (clientSecret $ appGoogleOAuth2Keys m)
        , FB.authFacebook ["email"]
        , authOpenId OPLocal []
        ]

    authHttpManager = getHttpManager

    loginHandler = lift $ defaultLayout $(widgetFile "login")

instance YF.YesodFacebook App where
    fbHttpManager = appHttpManager
    fbCredentials m =
        Credentials { appName="Fantasy"
                    , appId=(clientId $ appFacebookOAuth2Keys m)
                    , appSecret=(clientSecret $ appFacebookOAuth2Keys m)
                    }

instance YesodAuthPersist App

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage


--------
-- UI --
--------
alertClass :: Text -> Text
alertClass "" = "bg-primary"
alertClass status = "alert-" ++ status


--------------
-- Handlers --
--------------
unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = US.fakeHandlerGetLogger appLogger

backgroundHandler :: Handler () -> Handler ()
backgroundHandler = forkHandler $ $logErrorS "errorHandler" . tshow


-----------
-- Users --
-----------
isAdmin :: Maybe (Entity User) -> Bool
isAdmin (Just (Entity _ user)) = userIsAdmin user
isAdmin Nothing                = False


-------------
-- Leagues --
-------------
getLeaguesByUser :: Maybe UserId -> Handler [Entity League]
getLeaguesByUser maybeUserId = runDB
    $ E.select
    $ E.from $ \(team `E.InnerJoin` league) -> do
        E.on $ team ^. TeamLeagueId E.==. league ^. LeagueId
        E.where_ $
            team ^. TeamOwnerId E.==. E.val maybeUserId E.&&.
            league ^. LeagueIsActive E.==. E.val True
        E.orderBy [E.asc (league ^. LeagueName)]
        return league


-----------
-- Weeks --
-----------
getMostRecentWeek :: LeagueId -> SeasonId -> Handler (Entity Week)
getMostRecentWeek leagueId seasonId = runDB $ do
    maybeWeek <- selectFirst [ WeekLeagueId ==. leagueId
                             , WeekSeasonId <=. seasonId
                             ] [Desc WeekId]
    return $ fromJust maybeWeek

getMostRecentWeekId :: LeagueId -> SeasonId -> Handler WeekId
getMostRecentWeekId leagueId seasonId = do
    mostRecentWeekEntity <- getMostRecentWeek leagueId seasonId
    return $ entityKey mostRecentWeekEntity

-------------
-- Seasons --
-------------
readCurrentSeason :: LeagueId -> ReaderT SqlBackend Handler (Entity Season)
readCurrentSeason leagueId = do
    maybeSeason <- selectFirst [ SeasonLeagueId ==. leagueId
                               , SeasonIsActive ==. True
                               ] [Desc SeasonSeriesId]
    return $ fromJust maybeSeason

readCurrentSeasonId :: LeagueId -> ReaderT SqlBackend Handler SeasonId
readCurrentSeasonId leagueId = do
    currentSeasonEntity <- readCurrentSeason leagueId
    return $ entityKey currentSeasonEntity

getCurrentSeasonId :: LeagueId -> Handler SeasonId
getCurrentSeasonId leagueId = runDB $ readCurrentSeasonId leagueId

getSelectedSeason :: LeagueId -> Handler (Entity Season)
getSelectedSeason leagueId = do
    maybeSelectedSeasonId <- lookupSession "selectedSeasonId"
    case maybeSelectedSeasonId of
        Nothing -> setSelectedSeason leagueId Nothing
        Just selectedSeasonIdText ->
            case (fromPathPiece selectedSeasonIdText :: Maybe SeasonId) of
                Nothing -> setSelectedSeason leagueId Nothing
                Just selectedSeasonId -> do
                    selectedSeason <- runDB $ get404 selectedSeasonId
                    if seasonLeagueId selectedSeason == leagueId
                        then return $ Entity selectedSeasonId selectedSeason
                        else setSelectedSeason leagueId Nothing

getSelectedSeasonId :: LeagueId -> Handler SeasonId
getSelectedSeasonId leagueId = do
    selectedSeasonEntity <- getSelectedSeason leagueId
    return $ entityKey selectedSeasonEntity

setSelectedSeason :: LeagueId -> Maybe SeasonId -> Handler (Entity Season)
setSelectedSeason leagueId maybeSeasonId = do
    seasonEntity <- case maybeSeasonId of
        Nothing -> runDB $ readCurrentSeason leagueId
        Just seasonId -> do
            season <- runDB $ get404 seasonId
            return $ Entity seasonId season
    setSession "selectedSeasonId" $ toPathPiece $ entityKey seasonEntity
    return seasonEntity

getLastSeason :: Season -> Handler (Maybe (Entity Season))
getLastSeason season = runDB $ do
    series <- get404 $ seasonSeriesId season
    let (leagueId, year) = (seasonLeagueId season, seriesYear series - 1)
    getBy $ UniqueSeasonLeagueIdYear leagueId year

getLastSeasonId :: Season -> Handler (Maybe SeasonId)
getLastSeasonId season = do
    lastSeason <- getLastSeason season
    return $ map entityKey lastSeason


-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding
