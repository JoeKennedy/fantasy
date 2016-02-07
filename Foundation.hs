module Foundation where

import Import.NoFoundation

import qualified Database.Esqueleto as E
import           Database.Esqueleto ((^.))
import           Database.Persist.Sql                 (ConnectionPool, runSqlPool)
import           Facebook                             (Credentials(..))
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

        leagues <- runDB
            $ E.select
            $ E.from $ \(team `E.InnerJoin` league) -> do
                E.on $ team ^. TeamLeagueId E.==. league ^. LeagueId
                E.where_ (team ^. TeamOwnerId E.==. E.val maybeUserId)
                E.orderBy [E.asc (league ^. LeagueName)]
                return league

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
    isAuthorized (AuthR _)                   _    = return Authorized
    isAuthorized FaviconR                    _    = return Authorized
    isAuthorized RobotsR                     _    = return Authorized
    isAuthorized NewCharacterR               _    = requireAdmin
    isAuthorized (EditCharacterR _)          _    = requireAdmin
    isAuthorized HousesR                     True = requireAdmin
    isAuthorized (HouseR _)                  True = requireAdmin
    isAuthorized SpeciesListR                True = requireAdmin
    isAuthorized (SpeciesR _)                True = requireAdmin
    isAuthorized SeriesListR                 True = requireAdmin
    isAuthorized (SeriesR _)                 True = requireAdmin
    isAuthorized (SeriesEpisodesR _)         True = requireAdmin
    isAuthorized (SeriesEpisodeR _ _)        True = requireAdmin
    isAuthorized (SeriesEpisodeEventsR _ _)  _    = requireAdmin
    isAuthorized (SeriesEpisodeEventR _ _ _) _    = requireAdmin

    isAuthorized (LeagueR leagueId) _ = requirePublicOrTeamOwner leagueId
    isAuthorized (LeagueSettingsR leagueId _) True = requireLeagueManager leagueId
    -- TODO - change this to ensure user is logged in or a team owner
    isAuthorized (LeagueSettingsR leagueId _) False = requirePublicOrTeamOwner leagueId
    isAuthorized (SetupLeagueR _) _ = requireLoggedIn
    -- Default to Authorized for now.
    isAuthorized _ _ = return Authorized

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

requireLoggedIn :: (Typeable (AuthEntity master), PersistEntity (AuthEntity master)
                   , YesodAuthPersist master, AuthId master ~ Key (AuthEntity master)
                   , AuthEntity master ~ User) => HandlerT master IO AuthResult
requireLoggedIn = do
    mu <- maybeAuth
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just _  -> Authorized

requireAdmin :: (Typeable (AuthEntity master), PersistEntity (AuthEntity master)
           , YesodAuthPersist master, AuthId master ~ Key (AuthEntity master)
           , AuthEntity master ~ User) => HandlerT master IO AuthResult
requireAdmin = do
    mu <- maybeAuth
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just (Entity _ u) -> if userIsAdmin u then Authorized else Unauthorized "You must be an admin"

requirePublicOrTeamOwner :: (YesodPersist site, YesodAuth site,
                        YesodPersistBackend site ~ SqlBackend,
                        AuthId site ~ Key User) =>
                       LeagueId -> HandlerT site IO AuthResult
requirePublicOrTeamOwner leagueId = do
    league <- runDB $ get404 leagueId
    muid <- maybeAuthId
    case (leagueIsPrivate league, muid) of
        (False, _) -> return Authorized
        (True, Nothing) -> return AuthenticationRequired
        (True, Just uid) -> do
            teams <- runDB $ selectList [TeamLeagueId ==. leagueId] []
            case find (\(Entity _ t) -> teamOwnerId t == Just uid) teams of
                Just _  -> return Authorized
                Nothing -> return $ Unauthorized "This league is private"

-- TODO - this function will need to change later so that someone
-- other than the creator can manage the league
-- In addition, this should be used when determining
-- whether or not users can see fields for the settings
-- Easiest way to start might be to just disable all fields
-- if the user can't manage the league
requireLeagueManager :: (YesodPersist site, YesodAuth site, AuthId site ~ Key User,
                    YesodPersistBackend site ~ SqlBackend) =>
                   LeagueId -> HandlerT site IO AuthResult
requireLeagueManager leagueId = do
    league <- runDB $ get404 leagueId
    muid <- maybeAuthId
    return $ case muid of
        Just uid ->
            if uid == leagueCreatedBy league
                then Authorized
                else Unauthorized "You aren't allowed to manage this league"
        Nothing -> AuthenticationRequired


instance YesodBreadcrumbs App where
    breadcrumb HomeR = return ("Home", Nothing)
    breadcrumb (AuthR LoginR) = return ("Sign In", Just HomeR)

    -- Character, species, and house breadcrumbs
    breadcrumb CharactersR = return ("Characters", Just HomeR)
    breadcrumb NewCharacterR = return ("New", Just CharactersR)
    breadcrumb (CharacterR characterId) = do
        character <- runDB $ get404 characterId
        return (characterName character, Just CharactersR)
    breadcrumb (EditCharacterR characterId) = return ("Edit", Just $ CharacterR characterId)

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

    breadcrumb (LeagueTeamsR leagueId) = return ("Teams", Just $ LeagueR leagueId)
    breadcrumb (LeagueTeamR leagueId teamId) = do
        team <- runDB $ get404 teamId
        return ("House " ++ teamName team, Just $ LeagueTeamsR leagueId)
    breadcrumb (LeagueTeamSettingsR leagueId teamId) = return ("Settings", Just $ LeagueTeamR leagueId teamId)

    breadcrumb (LeagueSettingsR leagueId LeagueEditSettingsR) = return ("League Settings", Just $ LeagueR leagueId)
    breadcrumb (LeagueSettingsR leagueId LeagueGeneralSettingsR) = return ("General Settings", Just $ LeagueR leagueId)
    breadcrumb (LeagueSettingsR leagueId LeagueScoringSettingsR) = return ("Scoring Settings", Just $ LeagueR leagueId)
    breadcrumb (LeagueSettingsR leagueId LeagueDraftSettingsR) = return ("Draft Settings", Just $ LeagueR leagueId)
    breadcrumb (LeagueSettingsR leagueId LeagueTeamsSettingsR) = return ("Team Settings", Just $ LeagueR leagueId)

    breadcrumb (SetupLeagueR SetupNewLeagueR)       = return ("Setup", Just LeaguesR)
    breadcrumb (SetupLeagueR SetupGeneralSettingsR) = return ("General Settings", Just $ SetupLeagueR SetupNewLeagueR)
    breadcrumb (SetupLeagueR SetupScoringSettingsR) = return ("Scoring Settings", Just $ SetupLeagueR SetupGeneralSettingsR)
    breadcrumb (SetupLeagueR SetupDraftSettingsR)   = return ("Draft Settings", Just $ SetupLeagueR SetupScoringSettingsR)
    breadcrumb (SetupLeagueR SetupTeamsSettingsR)    = return ("Team Settings", Just $ SetupLeagueR SetupDraftSettingsR)
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
            (Just (Entity _ ident), Just (Entity uid _)) -> do
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

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding
