module Foundation where

import Import.NoFoundation
import Database.Persist.Sql    (ConnectionPool, runSqlPool)
import Facebook                (Credentials(..))
import Text.Hamlet             (hamletFile)
import Text.Jasmine            (minifym)
import qualified Yesod.Auth.GoogleEmail2 as GE
import qualified Yesod.Facebook as YF
import qualified Yesod.Auth.Facebook.ServerSide as FB
import Yesod.Auth.OpenId
import Yesod.Default.Util      (addStaticContentExternal)
import Yesod.Core.Types        (Logger)
import qualified Yesod.Core.Unsafe as Unsafe

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

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    defaultLayout widget = do
        master <- getYesod
        maybeUser <- maybeAuth
        mmsg <- getMessage
        (title', parents) <- breadcrumbs

        seriesList <- runDB $ selectList [] [Asc SeriesNumber]

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
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- Routes not requiring authentication.
    isAuthorized (AuthR _) _ = return Authorized
    isAuthorized FaviconR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized
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
                    , userLastName = Nothing
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
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding
