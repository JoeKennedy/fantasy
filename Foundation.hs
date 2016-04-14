module Foundation where

import Import.NoFoundation

import qualified Database.Esqueleto as E
import           Database.Esqueleto ((^.))
import           Database.Persist.Sql                 (ConnectionPool, runSqlPool)
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
        maybeUserId <- maybeAuthId
        mmsg <- getMessage
        (title', parents) <- breadcrumbs

        seriesList <- runDB $ selectList [] [Asc SeriesNumber]

        leagues <- getLeaguesByUser maybeUserId

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            addStylesheet $ StaticR css_bootstrap_css
            addStylesheet $ StaticR css_font_awesome_css
            addStylesheet $ StaticR css_bootstrap_social_css
            addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/2.1.0/jquery.min.js"
            addScript $ StaticR js_bootstrap_js
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

    isAuthorized CharactersR                 _      = return Authorized
    isAuthorized (CharacterR _)              _      = return Authorized
    isAuthorized NewCharacterR               _      = requireAdmin
    isAuthorized (EditCharacterR _)          _      = requireAdmin
    isAuthorized (CharacterBlurbsR _)        _      = requireAdmin
    isAuthorized (CharacterBlurbR _ _)       _      = requireAdmin
    isAuthorized HousesR                     isPost = requireAdminIfPost isPost
    isAuthorized (HouseR _)                  isPost = requireAdminIfPost isPost
    isAuthorized SpeciesListR                isPost = requireAdminIfPost isPost
    isAuthorized (SpeciesR _)                isPost = requireAdminIfPost isPost
    isAuthorized SeriesListR                 isPost = requireAdminIfPost isPost
    isAuthorized (SeriesR _)                 isPost = requireAdminIfPost isPost
    isAuthorized (SeriesEpisodesR _)         isPost = requireAdminIfPost isPost
    isAuthorized (SeriesEpisodeR _ _)        isPost = requireAdminIfPost isPost
    isAuthorized (SeriesEpisodeEventsR _ _)  _      = requireAdmin
    isAuthorized (SeriesEpisodeEventR _ _ _) _      = requireAdmin

    -- TODO - For team and player routes, make sure that each
    -- entity is in the correct league
    isAuthorized LeaguesR                          _ = return Authorized
    isAuthorized (LeagueR leagueId)                _ = requirePublicOrLeagueMember leagueId
    isAuthorized (LeagueCancelR leagueId)          _ = requireLeagueManager leagueId
    isAuthorized (LeagueDraftR leagueId draftYear) _ = requireLeagueManagerAndIncompleteDraft leagueId draftYear
    isAuthorized (LeagueTransactionsR leagueId)    _ = requirePublicOrLeagueMember leagueId
    isAuthorized (LeagueAcceptTradeR _ tid)        _ = requireTradeAcceptable tid
    isAuthorized (LeagueDeclineTradeR _ tid)       _ = requireTradeDeclinable tid
    isAuthorized (LeagueTeamsR leagueId)           _ = requirePublicOrLeagueMember leagueId
    isAuthorized (LeagueTeamR leagueId _)          _ = requirePublicOrLeagueMember leagueId
    isAuthorized (LeagueTeamJoinR _ tid verKey) True = requireCorrectVerKeyAndLoggedIn tid verKey
    isAuthorized (LeagueTeamJoinR _ tid vkey)  False = requireCorrectVerificationKey tid vkey
    isAuthorized (LeagueTeamSettingsR _ teamId)    _ = requireTeamOwner teamId

    isAuthorized (LeaguePlayersR leagueId)         _ = requirePublicOrLeagueMember leagueId
    isAuthorized (LeaguePlayerR leagueId _)        _ = requirePublicOrLeagueMember leagueId
    isAuthorized (LeaguePlayerStartR _ playerId)   _ = requirePlayerOwner playerId
    isAuthorized (LeaguePlayerBenchR _ playerId)   _ = requirePlayerOwner playerId
    isAuthorized (LeaguePlayerClaimR _ _ playerId) _ = requirePlayerOwner playerId
    isAuthorized (LeaguePlayerTradeR _ _ playerId) _ = requirePlayerOwner playerId

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

requireTradeAcceptable :: TransactionId -> Handler AuthResult
requireTradeAcceptable transactionId = do
    transaction <- runDB $ get404 transactionId
    muid <- maybeAuthId
    case (muid, transactionOtherTeamId transaction) of
        (Nothing, _)     -> return AuthenticationRequired
        (_, Nothing)     -> return $ Unauthorized "Transaction must have another team"
        (_, Just teamId) ->
            if transactionStatus transaction == Requested && transactionType transaction == Trade
                then requireTeamOwner teamId
                else return $ Unauthorized "Must be a trade transaction with status of requested"

requireTradeDeclinable :: TransactionId -> Handler AuthResult
requireTradeDeclinable = requireTradeAcceptable

requireCorrectVerificationKey :: TeamId -> Text -> Handler AuthResult
requireCorrectVerificationKey teamId verificationKey = do
    team <- runDB $ get404 teamId
    return $ if teamVerificationKey team == verificationKey
        then Authorized
        else Unauthorized "You aren't allowed to join this league"

requireCorrectVerKeyAndLoggedIn :: TeamId -> Text -> Handler AuthResult
requireCorrectVerKeyAndLoggedIn teamId verKey = do
    authResult <- requireLoggedIn
    case authResult of Authorized -> requireCorrectVerificationKey teamId verKey
                       _ -> return authResult

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

requireLeagueManagerAndIncompleteDraft :: LeagueId -> Int -> Handler AuthResult
requireLeagueManagerAndIncompleteDraft leagueId draftYear
    | draftYear /= 2016 = return $ Unauthorized "Draft can't happen for that year"
    | otherwise = do
        league <- runDB $ get404 leagueId
        authResult <- requireLeagueManager leagueId
        return $ case authResult of
            Authorized -> if leagueIsDraftComplete league
                              then Unauthorized "Draft already completed"
                              else Authorized
            _ -> authResult

requirePlayerOwner :: PlayerId -> Handler AuthResult
requirePlayerOwner playerId = do
    player <- runDB $ get404 playerId
    muid <- maybeAuthId
    let unauthorized = Unauthorized "This player is not on your team"
    case (muid, playerTeamId player) of
        (Nothing, _) -> return AuthenticationRequired
        (_, Nothing) -> return unauthorized
        (_, Just teamId) -> do
            team <- runDB $ get404 teamId
            return $ if teamOwnerId team == muid then Authorized else unauthorized


instance YesodBreadcrumbs App where
    breadcrumb HomeR = return ("Home", Nothing)
    breadcrumb (AuthR LoginR) = return ("Sign In", Just HomeR)
    breadcrumb FAQR = return ("How To Play", Just HomeR)

    -- Character, species, and house breadcrumbs
    breadcrumb CharactersR = return ("Characters", Just HomeR)
    breadcrumb NewCharacterR = return ("New", Just CharactersR)
    breadcrumb (CharacterR characterId) = do
        character <- runDB $ get404 characterId
        return (characterName character, Just CharactersR)
    breadcrumb (EditCharacterR characterId) = return ("Edit", Just $ CharacterR characterId)
    breadcrumb (CharacterBlurbsR characterId) = return ("Blurbs", Just $ CharacterR characterId)
    breadcrumb (CharacterBlurbR characterId blurbId) = return (toPathPiece blurbId, Just $ CharacterBlurbsR characterId)

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
    breadcrumb (SeriesEpisodeEventR seriesNo episodeNo eventId) =
        return ("Event #" ++ toPathPiece eventId, Just $ SeriesEpisodeR seriesNo episodeNo)

    -- League breadcrumbs
    breadcrumb LeaguesR           = return ("Leagues", Just HomeR)
    breadcrumb (LeagueR leagueId) = do
        league <- runDB $ get404 leagueId
        return (leagueName league, Just LeaguesR)
    breadcrumb (LeagueCancelR leagueId)         = return ("Cancel", Just $ LeagueR leagueId)
    breadcrumb (LeagueDraftR leagueId _)        = return ("Draft",        Just $ LeagueR leagueId)
    breadcrumb (LeagueTransactionsR leagueId)   = return ("Transactions", Just $ LeagueR leagueId)
    breadcrumb (LeagueAcceptTradeR leagueId _)  = return ("Accept Trade", Just $ LeagueTransactionsR leagueId)
    breadcrumb (LeagueDeclineTradeR leagueId _) = return ("Decline Trade", Just $ LeagueTransactionsR leagueId)

    -- League players breadcrumbs
    breadcrumb (LeaguePlayersR leagueId) = return ("Characters", Just $ LeagueR leagueId)
    breadcrumb (LeaguePlayerR leagueId playerId) = do
        player <- runDB $ get404 playerId
        character <- runDB $ get404 $ playerCharacterId player
        return (characterName character, Just $ LeagueR leagueId)
    breadcrumb (LeaguePlayerStartR leagueId playerId)   = return ("Start", Just $ LeaguePlayerR leagueId playerId)
    breadcrumb (LeaguePlayerBenchR leagueId playerId)   = return ("Bench", Just $ LeaguePlayerR leagueId playerId)
    breadcrumb (LeaguePlayerClaimR leagueId playerId _) = return ("Claim", Just $ LeaguePlayerR leagueId playerId)
    breadcrumb (LeaguePlayerTradeR leagueId playerId _) = return ("Trade", Just $ LeaguePlayerR leagueId playerId)

    -- League teams breadcrumbs
    breadcrumb (LeagueTeamsR leagueId) = return ("Houses", Just $ LeagueR leagueId)
    breadcrumb (LeagueTeamR leagueId teamId) = do
        team <- runDB $ get404 teamId
        return ("House " ++ teamName team, Just $ LeagueTeamsR leagueId)
    breadcrumb (LeagueTeamSettingsR leagueId teamId) = return ("Settings", Just $ LeagueTeamR leagueId teamId)
    breadcrumb (LeagueTeamJoinR leagueId teamId _)   = return ("Join", Just $ LeagueTeamR leagueId teamId)

    -- League settings breadcrumbs
    breadcrumb (LeagueSettingsR leagueId LeagueEditSettingsR)    = return ("League Settings", Just $ LeagueR leagueId)
    breadcrumb (LeagueSettingsR leagueId LeagueGeneralSettingsR) = return ("General Settings", Just $ LeagueR leagueId)
    breadcrumb (LeagueSettingsR leagueId LeagueScoringSettingsR) = return ("Scoring Settings", Just $ LeagueR leagueId)
    breadcrumb (LeagueSettingsR leagueId LeagueDraftSettingsR)   = return ("Draft Settings", Just $ LeagueR leagueId)
    breadcrumb (LeagueSettingsR leagueId LeagueTeamsSettingsR)   = return ("Team Settings", Just $ LeagueR leagueId)

    -- League setup breadcrumbs
    breadcrumb (SetupLeagueR SetupNewLeagueR)       = return ("Setup", Just LeaguesR)
    breadcrumb (SetupLeagueR SetupGeneralSettingsR) = return ("General Settings", Just $ SetupLeagueR SetupNewLeagueR)
    breadcrumb (SetupLeagueR SetupScoringSettingsR) = return ("Scoring Settings", Just $ SetupLeagueR SetupGeneralSettingsR)
    breadcrumb (SetupLeagueR SetupDraftSettingsR)   = return ("Draft Settings", Just $ SetupLeagueR SetupScoringSettingsR)
    breadcrumb (SetupLeagueR SetupTeamsSettingsR)   = return ("Team Settings", Just $ SetupLeagueR SetupDraftSettingsR)
    breadcrumb (SetupLeagueR SetupConfirmSettingsR) = return ("Complete Setup", Just $ SetupLeagueR SetupTeamsSettingsR)

    -- These pages never call breadcrumb
    breadcrumb StaticR{}              = return ("", Nothing)
    breadcrumb AuthR{}                = return ("", Nothing)
    breadcrumb FaviconR               = return ("", Nothing)
    breadcrumb RobotsR                = return ("", Nothing)
    breadcrumb SeriesEpisodeEventsR{} = return ("", Nothing)

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

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = US.fakeHandlerGetLogger appLogger

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

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding
