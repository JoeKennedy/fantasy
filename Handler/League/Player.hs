module Handler.League.Player where

import Import
import Handler.Common             (extractValue, groupByThirdOfFive)
import Handler.League.Layout
import Handler.League.Transaction

import           Data.Maybe         (fromJust)
import qualified Database.Esqueleto as E
import           Database.Esqueleto ((^.), (?.))
import           Text.Blaze         (toMarkup)

------------
-- Routes --
------------
getLeaguePlayersR :: LeagueId -> Handler Html
getLeaguePlayersR leagueId = do
    league <- runDB $ get404 leagueId
    let leagueEntity = Entity leagueId league
    maybeTeamId <- maybeAuthTeamId leagueId
    Entity _ generalSettings <- runDB $ getBy404 $ UniqueGeneralSettingsLeagueId leagueId
    leaguePlayers <- getPlayers leagueId
    allPlayers <- playersWithButtons leagueEntity leaguePlayers
    currentTeamPlayers <- getTeamPlayers maybeTeamId
    myPlayers <- playersWithButtons leagueEntity currentTeamPlayers
    let (freeAgents, onRosters) = partition (\(_, _, maybeTeam, _, _) -> isNothing maybeTeam) allPlayers
        teamsAndPlayers = groupByThirdOfFive $ sortByTeam onRosters
        numberOfStarters = generalSettingsNumberOfStarters generalSettings
        rosterSize = generalSettingsRosterSize generalSettings
    leagueLayout leagueId "Players" $(widgetFile "league/players")

getLeaguePlayerR :: LeagueId -> PlayerId -> Handler ()
getLeaguePlayerR leagueId playerId = do
    player <- runDB $ get404 playerId
    redirect $ case playerTeamId player of Just teamId -> LeagueTeamR leagueId teamId
                                           Nothing -> LeaguePlayersR leagueId

postLeaguePlayerStartR :: LeagueId -> PlayerId -> Handler ()
postLeaguePlayerStartR leagueId playerId = do
    player <- runDB $ get404 playerId
    transactionId <- singlePlayerTransaction (Entity playerId player) Start
    didAutoFail <- autoFailStartTransaction leagueId transactionId player
    if didAutoFail then return () else do
        Entity _ generalSettings <- runDB $ getBy404 $ UniqueGeneralSettingsLeagueId leagueId
        let teamId = fromJust $ playerTeamId player
        team <- runDB $ get404 teamId
        character <- runDB $ get404 $ playerCharacterId player
        if teamStartersCount team >= generalSettingsNumberOfStarters generalSettings
            then do
                let message = "You need to bench another player before starting "
                              ++ characterName character
                failTransaction_ transactionId "Can't start player when no slots are available"
                setMessage $ toMarkup message
            else do
                now <- liftIO getCurrentTime
                userId <- requireAuthId
                runDB $ update playerId [PlayerIsStarter =. True, PlayerUpdatedAt =. now,
                                         PlayerUpdatedBy =. userId]
                runDB $ update teamId [TeamStartersCount +=. 1, TeamUpdatedAt =. now,
                                       TeamUpdatedBy =. userId]
                succeedTransaction transactionId
                let message = characterName character ++ " is now starting in your lineup."
                setMessage $ toMarkup message

postLeaguePlayerBenchR :: LeagueId -> PlayerId -> Handler ()
postLeaguePlayerBenchR leagueId playerId = do
    player <- runDB $ get404 playerId
    transactionId <- singlePlayerTransaction (Entity playerId player) Bench
    didAutoFail <- autoFailBenchTransaction leagueId transactionId player
    if didAutoFail then return () else do
        now <- liftIO getCurrentTime
        userId <- requireAuthId
        Entity _ generalSettings <- runDB $ getBy404 $ UniqueGeneralSettingsLeagueId leagueId
        team <- runDB $ get404 $ fromJust $ playerTeamId player
        runDB $ update playerId [PlayerIsStarter =. False, PlayerUpdatedAt =. now,
                                 PlayerUpdatedBy =. userId]
        runDB $ update (fromJust $ playerTeamId player) [TeamStartersCount -=. 1, TeamUpdatedAt =. now,
                                                         TeamUpdatedBy =. userId]
        character <- runDB $ get404 $ playerCharacterId player
        let spotsLeft = generalSettingsNumberOfStarters generalSettings - teamStartersCount team + 1
            message = characterName character ++ " is now benched in your lineup and you have "
                      ++ pack (show spotsLeft) ++ " spots for starters."
        succeedTransaction transactionId
        setMessage $ toMarkup message

postLeaguePlayerClaimR :: LeagueId -> PlayerId -> PlayerId -> Handler ()
postLeaguePlayerClaimR leagueId playerIdToAdd playerIdToDrop = do
    playerToAdd  <- runDB $ get404 playerIdToAdd
    playerToDrop <- runDB $ get404 playerIdToDrop
    characterToAdd  <- runDB $ get404 $ playerCharacterId playerToAdd
    characterToDrop <- runDB $ get404 $ playerCharacterId playerToDrop
    transactionId <- twoPlayerTransaction (Entity playerIdToAdd playerToAdd)
                     (Entity playerIdToDrop playerToDrop) Claim
    didAutoFail <- autoFailClaimTransaction leagueId transactionId playerToAdd playerToDrop
    if didAutoFail then return () else setMessage $ toMarkup $
        "Claim to pick up " ++ characterName characterToAdd ++ " and drop " ++
        characterName characterToDrop ++ " will be processed at 9am UTC."

postLeaguePlayerTradeR :: LeagueId -> PlayerId -> PlayerId -> Handler ()
postLeaguePlayerTradeR leagueId playerIdToTake playerIdToGive = do
    playerToTake <- runDB $ get404 playerIdToTake
    playerToGive <- runDB $ get404 playerIdToGive
    transactionId <- twoPlayerTransaction (Entity playerIdToTake playerToTake)
                     (Entity playerIdToGive playerToGive) Trade
    didAutoFail <- autoFailTradeTransaction leagueId transactionId playerToTake playerToGive
    if didAutoFail then return () else do
        otherTeam <- runDB $ get404 $ fromJust $ playerTeamId playerToTake
        characterToTake <- runDB $ get404 $ playerCharacterId playerToTake
        characterToGive <- runDB $ get404 $ playerCharacterId playerToGive
        setMessage $ toMarkup $ "Trade to give " ++ characterName characterToGive ++
            " to House " ++ teamName otherTeam ++ " in exchange for " ++
            characterName characterToTake ++ " has been submitted. It is up to House " ++
            teamName otherTeam ++ " to accept or deny the trade."

-------------
-- Widgets --
-------------
playersTable :: [(Int, Entity Player, Maybe (Entity Team), Entity Character, Widget)] ->
                PlayersTableType -> Widget
playersTable players playersTableType =
    let colspan = playersTableColumnCount playersTableType
        subTables = case splitPlayersTable playersTableType of
            Nothing -> [(packPlayersTableType playersTableType, players, [])]
            Just (numberOfStarters, rosterSize) ->
                splitStartersAndBench players numberOfStarters rosterSize
    in  $(widgetFile "league/players_table")

playersModal :: [(Int, Entity Player, Maybe (Entity Team), Entity Character, Widget)] ->
                PlayersTableType -> TransactionType -> Widget
playersModal players playersTableType modalType =
    let typeText = toPathPiece modalType
        typeLower = toLower typeText
        modalId = typeLower ++ "_modal"
        modalLabelId = modalId ++ "_label"
        buttonId = "submit_" ++ typeLower
    in  $(widgetFile "league/players_modal")

-------------
-- Queries --
-------------
getPlayers :: LeagueId -> Handler [(Entity Player, Maybe (Entity Team), Entity Character)]
getPlayers leagueId = runDB
    $ E.select
    $ E.from $ \(player `E.InnerJoin` character `E.LeftOuterJoin` team) -> do
        E.on $ E.just (player ^. PlayerTeamId) E.==. E.just (team ?. TeamId)
        E.on $ player ^. PlayerCharacterId E.==. character ^. CharacterId
        E.where_ $ player ^. PlayerLeagueId E.==. E.val leagueId
        E.orderBy [E.asc (character ^. CharacterName)]
        return (player, team, character)

getTeamPlayers :: Maybe TeamId -> Handler [(Entity Player, Maybe (Entity Team), Entity Character)]
getTeamPlayers Nothing = return []
getTeamPlayers (Just teamId) = runDB
    $ E.select
    $ E.from $ \(player `E.InnerJoin` character `E.LeftOuterJoin` team) -> do
        E.on $ E.just (player ^. PlayerTeamId) E.==. E.just (team ?. TeamId)
        E.on $ player ^. PlayerCharacterId E.==. character ^. CharacterId
        E.where_ $ player ^. PlayerTeamId E.==. E.just (E.val teamId)
        E.orderBy [ E.desc (player ^. PlayerIsStarter)
                  , E.asc (character ^. CharacterName)
                  ]
        return (player, team, character)

--------------------------
-- Player Action Button --
--------------------------
playersWithButtons :: Entity League -> [(Entity Player, Maybe (Entity Team), Entity Character)] ->
                      Handler [(Int, Entity Player, Maybe (Entity Team), Entity Character, Widget)]
playersWithButtons (Entity leagueId league) players = do
    maybeUserId <- maybeAuthId
    isUserLeagueMember <- isLeagueMember maybeUserId leagueId
    let isDraftComplete = leagueIsDraftComplete league
        playersAndButtons = if isUserLeagueMember && isDraftComplete
            then map (\(p, mt, c) -> playerWithButton p mt c (fromJust maybeUserId)) players
            else map (\(p, mt, c) -> (p, mt, c, [whamlet||])) players
    return $ zipWith (\n (a,b,c,d) -> (n,a,b,c,d)) [1..] playersAndButtons

playerWithButton :: Entity Player -> Maybe (Entity Team) -> Entity Character -> UserId ->
                    (Entity Player, Maybe (Entity Team), Entity Character, Widget)
playerWithButton (Entity playerId player) maybeTeam character userId =
    let (text, dataToggle, dataTarget, icon) = playerButtonAttributes (Entity playerId player) maybeTeam userId
        buttonId = text ++ "-" ++ toPathPiece (playerLeagueId player) ++ "-" ++ toPathPiece playerId
        name = characterName $ extractValue character
    in  (Entity playerId player, maybeTeam, character, $(widgetFile "league/player_action_button"))

playerButtonAttributes :: Entity Player -> Maybe (Entity Team) -> UserId -> (Text, Text, Text, Text)
playerButtonAttributes _ Nothing _ =
    ("claim", "modal", "#claim_modal", "plus")
playerButtonAttributes (Entity _ player) (Just (Entity _ team)) userId =
    if isTeamOwner (Just userId) (Just team) then (if playerIsStarter player
        then ("bench", "", "", "level-down")
        else ("start", "", "", "level-up"))
    else ("trade", "modal", "#trade_modal", "refresh")

-------------
-- Helpers --
-------------
recombineTeamToPlayers :: Entity Team -> [(Int, Entity Player, Entity Character, Widget)] ->
                          [(Int, Entity Player, Maybe (Entity Team), Entity Character, Widget)]
recombineTeamToPlayers teamEntity players =
    map (\(num, player, character, widget) -> (num, player, Just teamEntity, character, widget)) players

isLeagueMember :: Maybe UserId -> LeagueId -> Handler Bool
isLeagueMember Nothing _ = return False
isLeagueMember justUserId leagueId = do
    teams <- runDB $ selectList [TeamLeagueId ==. leagueId] []
    return $ isJust $ find (\(Entity _ t) -> teamOwnerId t == justUserId) teams

splitStartersAndBench :: [(Int, Entity Player, Maybe (Entity Team), Entity Character, Widget)] -> Int -> Int ->
                         [(Text, [(Int, Entity Player, Maybe (Entity Team), Entity Character, Widget)], [Int])]
splitStartersAndBench players numberOfStarters rosterSize =
    let (starters, bench) = partition (\(_, Entity _ p, _, _, _) -> playerIsStarter p) players
        benchWithNumbers = map (\(n, p, mt, c, w) -> (n - numberOfStarters, p, mt, c, w)) bench
        extraStarterSlots = [length starters + 1 .. numberOfStarters]
        extraBenchSlots   = [length bench + 1 .. rosterSize - numberOfStarters]
    in  [("Starters", starters, extraStarterSlots), ("Bench", benchWithNumbers, extraBenchSlots)]

maybeAuthTeamId :: LeagueId -> Handler (Maybe TeamId)
maybeAuthTeamId leagueId = do
    maybeUserId <- maybeAuthId
    maybeTeam <- runDB $ selectFirst [TeamLeagueId ==. leagueId, TeamOwnerId ==. maybeUserId] []
    return $ case (maybeUserId, maybeTeam) of
        (Just _, Just (Entity teamId _)) -> Just teamId
        (_, _) -> Nothing

sortByTeam :: [(Int, Entity Player, Maybe (Entity Team), Entity Character, Widget)] ->
              [(Int, Entity Player, Entity Team, Entity Character, Widget)]
sortByTeam players =
    let withTeams = map (\(n, p, mt, c, w) -> (n, p, fromJust mt, c, w)) players
    in  sortBy (\(_, _, Entity _ t1, _, _) (_, _, Entity _ t2, _, _) -> teamName t1 `compare` teamName t2) withTeams

