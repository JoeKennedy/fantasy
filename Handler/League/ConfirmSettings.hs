module Handler.League.ConfirmSettings where

import Import
import Handler.League.Setup

import qualified Data.ByteString.Lazy.UTF8 as LU
import           Network.Mail.Mime
import           Network.Mail.Mime.SES
import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)

------------
-- Routes --
------------
getSetupConfirmSettingsR :: Handler Html
getSetupConfirmSettingsR = do
    userId <- requireAuthId
    let action = SetupLeagueR SetupConfirmSettingsR
    (Entity leagueId league, lastCompletedStep) <- leagueOrRedirect userId action
    defaultLayout $ do
        let widget = $(widgetFile "league/confirm_setup")
            enctype = UrlEncoded
            maybeLeagueId = Just leagueId
        setTitle $ leagueSetupStepTitle league action
        $(widgetFile "layouts/league-setup-layout")

postSetupConfirmSettingsR :: Handler Html
postSetupConfirmSettingsR = do
    userId <- requireAuthId
    let action = SetupLeagueR SetupConfirmSettingsR
    (Entity leagueId league, _) <- leagueOrRedirect userId action
    teams <- runDB $ selectList [TeamLeagueId ==. leagueId] [Asc TeamNumber]
    case uncons teams of
        Nothing -> error "This league has no teams"
        Just (leagueManagerTeam, unjoinedTeams) -> do
            mapM_ (sendJoinEmail (Entity leagueId league) leagueManagerTeam) unjoinedTeams
            sendSetupSuccessEmail (Entity leagueId league) leagueManagerTeam
    updateLeagueLastCompletedStep leagueId league 6
    redirect $ LeagueR leagueId

-------------
-- Helpers --
-------------
-- TODO - make a new Handler for emails
sendJoinEmail :: Entity League -> Entity Team -> Entity Team -> Handler ()
sendJoinEmail (Entity leagueId league) (Entity _ leagueManagerTeam) (Entity _ team) = do
    master <- getYesod
    render <- getUrlRender
    let email = teamOwnerEmail team
        number = teamNumber team
        url = render $ LeagueTeamJoinR leagueId number $ teamVerificationKey team
    renderSendMailSES (appHttpManager master) (appSesCreds master email) Mail
        { mailHeaders =
            [ ( "Subject"
              , "You've been invited to join a fantasy league for HBO's Game Of Thrones"
              )
            ]
        , mailFrom = Address Nothing "grandmaester@fantasygameofthrones.com"
        , mailTo = [Address (Just $ teamOwnerName team) email]
        , mailCc = []
        , mailBcc = []
        , mailParts = return
            [ Part "text/plain" None Nothing [] $ LU.fromString $ unlines
                [ unpack (teamOwnerName leagueManagerTeam) ++
                    " has invited you to join an HBO's Game Of Thrones fantasy league "
                    ++ unpack (leagueName league)
                , ""
                , unpack url
                ]
            , Part "text/html" None Nothing [] $ renderHtml [shamlet|\
<p> #{teamOwnerName leagueManagerTeam} has invited you to join an HBO's Game Of
    \ Thrones fantasy league called #{leagueName league}.
<p>
    <a href="#{url}">Click here to join the league!
|]
            ]
        }

sendSetupSuccessEmail :: Entity League -> Entity Team -> Handler ()
sendSetupSuccessEmail (Entity leagueId league) (Entity _ leagueManagerTeam) = do
    master <- getYesod
    render <- getUrlRender
    let email = teamOwnerEmail leagueManagerTeam
        url = render $ LeagueR leagueId
    renderSendMailSES (appHttpManager master) (appSesCreds master email) Mail
        { mailHeaders =
            [ ( "Subject"
              , "You successfully set up a fantasy league for HBO's Game Of Thrones"
              )
            ]
        , mailFrom = Address Nothing "grandmaester@fantasygameofthrones.com"
        , mailTo = [Address (Just $ teamOwnerName leagueManagerTeam) email]
        , mailCc = []
        , mailBcc = []
        , mailParts = return
            [ Part "text/plain" None Nothing [] $ LU.fromString $ unlines
                [ "You have successfully created an HBO's Game Of Thrones fantasy league called "
                    ++ unpack (leagueName league)
                , ""
                , unpack url
                ]
            , Part "text/html" None Nothing [] $ renderHtml [shamlet|\
<p> You successfully created an HBO's Game Of Thrones fantasy league #
    called <a href="#{url}">#{leagueName league}</a>.
    Invitations to join the league have been sent to team owners.
|]
            ]
        }
