{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( getApplicationDev
    , appMain
    , develMain
    , makeFoundation
    -- * for DevelMain
    , getApplicationRepl
    , shutdownApp
    -- * for GHCI
    , handler
    , db
    ) where

import Control.Monad.Logger                 (liftLoc, runLoggingT)
import Database.Persist.Postgresql          (createPostgresqlPool, pgConnStr,
                                             pgPoolSize, runSqlPool)
import Import
import Language.Haskell.TH.Syntax           (qLocation)
import LoadEnv                              (loadEnv)
import Network.Mail.Mime.SES
import Network.Wai.Handler.Warp             (Settings, defaultSettings,
                                             defaultShouldDisplayException,
                                             runSettings, setHost,
                                             setOnException, setPort, getPort)
import Network.Wai.Middleware.RequestLogger (Destination (Logger),
                                             IPAddrSource (..),
                                             OutputFormat (..), destination,
                                             mkRequestLogger, outputFormat)
import System.Cron
import System.Environment                   (getEnv)
import System.Log.FastLogger                (defaultBufSize, newStdoutLoggerSet,
                                             toLogStr)

import qualified Data.ByteString.Char8 as S8
import qualified Data.Proxy as P
import qualified Web.ServerSession.Core as SS
import qualified Web.ServerSession.Backend.Persistent as SS

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import Handler.Common

import Handler.Admin.Blurb
import Handler.Admin.Character
import Handler.Admin.Common
import Handler.Admin.Episode
import Handler.Admin.Event
import Handler.Admin.House
import Handler.Admin.Score
import Handler.Admin.Series
import Handler.Admin.Species
import Handler.Character
import Handler.Episode
import Handler.Home
import Handler.House
import Handler.League
import Handler.League.ConfirmSettings
import Handler.League.DraftSettings
import Handler.League.GeneralSettings
import Handler.League.Player
import Handler.League.Scoring
import Handler.League.Team
import Handler.League.Transaction
import Handler.Series
import Handler.Species
import Handler.League.Week

-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp

-- Create migration function using both our entities and the
-- serversession-persistent-backend ones.
mkMigrate "migrateAll" (SS.serverSessionDefs (P.Proxy :: P.Proxy SS.SessionMap) ++ entityDefs)

-- | This function allocates resources (such as a database connection pool),
-- performs initialization and returns a foundation datatype value. This is also
-- the place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeFoundation :: AppSettings -> IO App
makeFoundation appSettings = do
    loadEnv
    -- Some basic initializations: HTTP connection manager, logger, and static
    -- subsite.
    appHttpManager <- newManager
    appLogger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger
    appStatic <-
        (if appMutableStatic appSettings then staticDevel else static)
        (appStaticDir appSettings)
    (appAcmeChallenge, appLetsEncrypt) <- getLetsEncrypt
    appFacebookOAuth2Keys <- getOAuth2Keys "FACEBOOK_OAUTH2_APP_ID" "FACEBOOK_OAUTH2_APP_SECRET"
    appGoogleOAuth2Keys <- getOAuth2Keys "GOOGLE_OAUTH2_CLIENT_ID" "GOOGLE_OAUTH2_CLIENT_SECRET"
    (appAmazonAccessKey, appAmazonSecretKey) <- getAmazonKeys
    let appSesCreds = \email -> SES { sesFrom = "grandmaester@fantasygameofthrones.com"
                                    , sesTo = [encodeUtf8 email]
                                    , sesAccessKey = S8.pack appAmazonAccessKey
                                    , sesSecretKey = S8.pack appAmazonSecretKey
                                    , sesRegion = usEast1
                                    }

    -- We need a log function to create a connection pool. We need a connection
    -- pool to create our foundation. And we need our foundation to get a
    -- logging function. To get out of this loop, we initially create a
    -- temporary foundation without a real connection pool, get a log function
    -- from there, and then create the real foundation.
    let mkFoundation appConnPool = App {..}
        -- The App {..} syntax is an example of record wild cards. For more
        -- information, see:
        -- https://ocharles.org.uk/blog/posts/2014-12-04-record-wildcards.html
        tempFoundation = mkFoundation $ error "connPool forced in tempFoundation"
        logFunc = messageLoggerSource tempFoundation appLogger

    -- Create the database connection pool
    -- TODO make the DB connect to DATABASE_URL if not in development
    pool <- flip runLoggingT logFunc $ createPostgresqlPool
        (pgConnStr  $ appDatabaseConf appSettings)
        (pgPoolSize $ appDatabaseConf appSettings)

    -- Perform database migration using our application's logging settings.
    runLoggingT (runSqlPool (runMigration migrateAll) pool) logFunc

    let foundation = mkFoundation pool

    threadIds <- execSchedule $ do
        addJob (unsafeHandler foundation processClaimRequests) "0 9 * * *" -- 9am UTC daily
        addJob (unsafeHandler foundation airEpisode)           "0 1 * * 1" -- 1am UTC Monday
        addJob (unsafeHandler foundation finishAiringEpisode)  "0 2 * * 1" -- 2am UTC Monday
    print threadIds

    -- Return the foundation
    return foundation

    where
        getOAuth2Keys :: String -> String -> IO OAuth2Keys
        getOAuth2Keys clientIdEnvVar clientSecretEnvVar = OAuth2Keys
            <$> fmap pack (getEnv clientIdEnvVar)
            <*> fmap pack (getEnv clientSecretEnvVar)

        getAmazonKeys :: IO (String, String)
        getAmazonKeys = do
            accessKey <- getEnv ("AWS_ACCESS_KEY")
            secretKey <- getEnv ("AWS_SECRET_KEY")
            return (accessKey, secretKey)

        getLetsEncrypt :: IO (Text, Text)
        getLetsEncrypt = do
            acmeChallenge <- getEnv "LETS_ENCRYPT_ACME_CHALLENGE"
            letsEncrypt   <- getEnv "LETS_ENCRYPT_SECRET"
            return (pack acmeChallenge, pack letsEncrypt)

-- | Convert our foundation to a WAI Application by calling @toWaiAppPlain@ and
-- applying some additional middlewares.
makeApplication :: App -> IO Application
makeApplication foundation = do
    logWare <- mkRequestLogger def
        { outputFormat =
            if appDetailedRequestLogging $ appSettings foundation
                then Detailed True
                else Apache
                        (if appIpFromHeader $ appSettings foundation
                            then FromFallback
                            else FromSocket)
        , destination = Logger $ loggerSet $ appLogger foundation
        }

    -- Create the WAI application and apply middlewares
    appPlain <- toWaiAppPlain foundation
    return $ logWare $ defaultMiddlewaresNoLogging appPlain

-- | Warp settings for the given foundation value.
warpSettings :: App -> Settings
warpSettings foundation =
      setPort (appPort $ appSettings foundation)
    $ setHost (appHost $ appSettings foundation)
    $ setOnException (\_req e ->
        when (defaultShouldDisplayException e) $ messageLoggerSource
            foundation
            (appLogger foundation)
            $(qLocation >>= liftLoc)
            "yesod"
            LevelError
            (toLogStr $ "Exception from Warp: " ++ show e))
      defaultSettings

-- | For yesod devel, return the Warp settings and WAI Application.
getApplicationDev :: IO (Settings, Application)
getApplicationDev = do
    settings <- getAppSettings
    foundation <- makeFoundation settings
    wsettings <- getDevSettings $ warpSettings foundation
    app <- makeApplication foundation
    return (wsettings, app)

getAppSettings :: IO AppSettings
getAppSettings = loadAppSettings [configSettingsYml] [] useEnv

-- | main function for use by yesod devel
develMain :: IO ()
develMain = develMainHelper getApplicationDev

-- | The @main@ function for an executable running this site.
appMain :: IO ()
appMain = do
    -- Get the settings from all relevant sources
    settings <- loadAppSettingsArgs
        -- fall back to compile-time values, set to [] to require values at runtime
        [configSettingsYmlValue]

        -- allow environment variables to override
        useEnv

    -- Generate the foundation from the settings
    foundation <- makeFoundation settings

    -- Generate a WAI Application from the foundation
    app <- makeApplication foundation

    -- Run the application with Warp
    runSettings (warpSettings foundation) app


--------------------------------------------------------------
-- Functions for DevelMain.hs (a way to run the app from GHCi)
--------------------------------------------------------------
getApplicationRepl :: IO (Int, App, Application)
getApplicationRepl = do
    settings <- getAppSettings
    foundation <- makeFoundation settings
    wsettings <- getDevSettings $ warpSettings foundation
    app1 <- makeApplication foundation
    return (getPort wsettings, foundation, app1)

shutdownApp :: App -> IO ()
shutdownApp _ = return ()


---------------------------------------------
-- Functions for use in development with GHCi
---------------------------------------------

-- | Run a handler
handler :: Handler a -> IO a
handler h = getAppSettings >>= makeFoundation >>= flip unsafeHandler h

-- | Run DB queries
db :: ReaderT SqlBackend (HandlerT App IO) a -> IO a
db = handler . runDB
