module Handler.League.Player where

import Import
import Handler.Common             (isAdmin, extractValue, groupByThirdOfSix)
import Handler.League.Layout
import Handler.League.Transaction
import Handler.League.Week        (getPerformancesForPlayer, getPlaysForPerformance)

import           Data.Maybe         (fromJust)
import qualified Database.Esqueleto as E
import           Database.Esqueleto ((^.), (?.))
import           Text.Blaze         (toMarkup)

-----------
-- Types --
-----------
type FullPlayer = (Entity Player, Maybe (Entity Team), Entity Character, Entity Series)
type FullPlayerForTable = (Int, Entity Player, Maybe (Entity Team),
                           Entity Character, Entity Series, Widget)
type FullPlayerWithButton = (Entity Player, Maybe (Entity Team),
                             Entity Character, Entity Series, Widget)
type FullPlayerForTableSansTeam = (Int, Entity Player, Entity Character,
                                   Entity Series, Widget)
type FullPlayerForTableReqTeam = (Int, Entity Player, Entity Team,
                                  Entity Character, Entity Series, Widget)

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
    let (freeAgents, onRosters) = partition (\(_, _, maybeTeam, _, _, _) -> isNothing maybeTeam) allPlayers
        teamsAndPlayers = groupByThirdOfSix $ sortByTeam onRosters
        numberOfStarters = generalSettingsNumberOfStarters generalSettings
        rosterSize = generalSettingsRosterSize generalSettings
    leagueLayout leagueId "Characters" $(widgetFile "league/players")

getLeaguePlayerR :: LeagueId -> PlayerId -> Handler Html
getLeaguePlayerR leagueId playerId = do
    maybeUser <- maybeAuth
    league <- runDB $ get404 leagueId
    player <- runDB $ get404 playerId
    let characterId = playerCharacterId player
    character <- runDB $ get404 characterId
    blurbs <- runDB $ selectList [BlurbCharacterId ==. characterId] [Desc BlurbId]
    performances <- getPerformancesForPlayer playerId
    playsByWeek <- mapM (\(p, _, _) -> getPlaysForPerformance p) performances
    let performancesAndPlays = zip performances playsByWeek
    maybeTeam <- case playerTeamId player of
        Nothing     -> return Nothing
        Just teamId -> do
            team <- runDB $ get404 teamId
            return $ Just $ Entity teamId team
    leagueLayout leagueId "Characters" $(widgetFile "league/player")

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
                updateTeamStartersCount teamId now userId
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
        let teamId = fromJust $ playerTeamId player
        runDB $ update playerId [PlayerIsStarter =. False, PlayerUpdatedAt =. now,
                                 PlayerUpdatedBy =. userId]
        updateTeamStartersCount teamId now userId
        character <- runDB $ get404 $ playerCharacterId player
        team <- runDB $ get404 teamId
        let spotsLeft = generalSettingsNumberOfStarters generalSettings - teamStartersCount team + 1
            message = characterName character ++ " is now benched in your lineup and you have "
                      ++ pack (show spotsLeft) ++ " spots for starters."
        succeedTransaction transactionId
        setMessage $ toMarkup message

postLeaguePlayerClaimR :: LeagueId -> PlayerId -> PlayerId -> Handler ()
postLeaguePlayerClaimR leagueId playerIdToAdd playerIdToDrop = do
    -- TODO - Remove this ASAP
    unpositionedClaimRequests <- runDB $ count [TransactionPosition ==. Nothing]
    if unpositionedClaimRequests > 0 then return () else do
        teamIds <- runDB $ selectKeysList [] [Asc TeamId]
        mapM_ repositionClaimRequests teamIds
    -- END of Remove this ASAP
    playerToAdd  <- runDB $ get404 playerIdToAdd
    playerToDrop <- runDB $ get404 playerIdToDrop
    characterToAdd  <- runDB $ get404 $ playerCharacterId playerToAdd
    characterToDrop <- runDB $ get404 $ playerCharacterId playerToDrop
    transactionId <- twoPlayerTransaction (Entity playerIdToAdd playerToAdd)
                     (Entity playerIdToDrop playerToDrop) Claim
    didAutoFail <- autoFailClaimTransaction leagueId transactionId playerToAdd playerToDrop
    if didAutoFail then return () else do
        transaction <- runDB $ get404 transactionId
        Entity _ generalSettings <- runDB $ getBy404 $ UniqueGeneralSettingsLeagueId leagueId
        let day = if transactionCreatedAt transaction == transactionProcessableAt transaction
                      then ""
                      else " " ++ (dayOfWeekToText $ generalSettingsWaiverPeriodInDays generalSettings + 1)
        setMessage $ toMarkup $
            "Claim to pick up " ++ characterName characterToAdd ++ " and drop " ++
            characterName characterToDrop ++ " will be processed" ++ day ++ " at 9am UTC."

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
playersTable :: [FullPlayerForTable] -> PlayersTableType -> Widget
playersTable players playersTableType =
    let colspan = playersTableColumnCount playersTableType
        subTables = case splitPlayersTable playersTableType of
            Nothing -> [(packPlayersTableType playersTableType, players, [])]
            Just (numberOfStarters, rosterSize) ->
                splitStartersAndBench players numberOfStarters rosterSize
    in  $(widgetFile "league/players_table")

playersModal :: [FullPlayerForTable] -> PlayersTableType -> TransactionType -> Widget
playersModal players playersTableType modalType =
    let typeText = toPathPiece modalType
        typeLower = toLower typeText
        modalId = typeLower ++ "_modal"
        modalLabelId = modalId ++ "_label"
        buttonId = "submit_" ++ typeLower
    in  $(widgetFile "league/players_modal")

blurbPanel :: Maybe (Entity User) -> CharacterId -> Entity Blurb -> Widget
blurbPanel maybeUser characterId (Entity blurbId blurb) = $(widgetFile "blurb_panel")

-------------
-- Queries --
-------------
getPlayers :: LeagueId -> Handler [FullPlayer]
getPlayers leagueId = runDB
    $ E.select
    $ E.from $ \(player `E.InnerJoin` character `E.LeftOuterJoin` team `E.InnerJoin` series) -> do
        E.on $ character ^. CharacterRookieSeriesId E.==. series ^. SeriesId
        E.on $ E.just (player ^. PlayerTeamId) E.==. E.just (team ?. TeamId)
        E.on $ player ^. PlayerCharacterId E.==. character ^. CharacterId
        E.where_ $ player ^. PlayerLeagueId E.==. E.val leagueId
             E.&&. player ^. PlayerIsPlayable E.==. E.val True
        E.orderBy [E.asc (character ^. CharacterName)]
        return (player, team, character, series)

getTeamPlayers :: Maybe TeamId -> Handler [FullPlayer]
getTeamPlayers Nothing = return []
getTeamPlayers (Just teamId) = runDB
    $ E.select
    $ E.from $ \(player `E.InnerJoin` character `E.LeftOuterJoin` team `E.InnerJoin` series) -> do
        E.on $ character ^. CharacterRookieSeriesId E.==. series ^. SeriesId
        E.on $ E.just (player ^. PlayerTeamId) E.==. E.just (team ?. TeamId)
        E.on $ player ^. PlayerCharacterId E.==. character ^. CharacterId
        E.where_ $ player ^. PlayerTeamId E.==. E.just (E.val teamId)
        E.orderBy [ E.desc (player ^. PlayerIsStarter)
                  , E.asc (character ^. CharacterName)
                  ]
        return (player, team, character, series)

--------------------------
-- Player Action Button --
--------------------------
playersWithButtons :: Entity League -> [FullPlayer] -> Handler [FullPlayerForTable]
playersWithButtons (Entity leagueId league) players = do
    maybeUserId <- maybeAuthId
    isUserLeagueMember <- isLeagueMember maybeUserId leagueId
    let isDraftComplete = leagueIsDraftComplete league
        playersAndButtons = if isUserLeagueMember && isDraftComplete
            then map (\(fullPlayer) -> playerWithButton fullPlayer (fromJust maybeUserId)) players
            else map (\(p, mt, c, s) -> (p, mt, c, s, [whamlet||])) players
    return $ zipWith (\n (a,b,c,d,e) -> (n,a,b,c,d,e)) [1..] playersAndButtons

playerWithButton :: FullPlayer -> UserId -> FullPlayerWithButton
playerWithButton ((Entity playerId player), maybeTeam, character, series) userId =
    let (text, dataToggle, dataTarget, icon) = playerButtonAttributes (Entity playerId player) maybeTeam userId
        buttonId = text ++ "-" ++ toPathPiece (playerLeagueId player) ++ "-" ++ toPathPiece playerId
        name = characterName $ extractValue character
    in  (Entity playerId player, maybeTeam, character, series, $(widgetFile "league/player_action_button"))

playerButtonAttributes :: Entity Player -> Maybe (Entity Team) -> UserId -> (Text, Text, Text, Text)
playerButtonAttributes _ Nothing _ = ("claim", "modal", "#claim_modal", "plus")
playerButtonAttributes (Entity _ player) (Just (Entity _ team)) userId =
    if isTeamOwner (Just userId) (Just team) then (if playerIsStarter player
        then ("bench", "", "", "level-down")
        else ("start", "", "", "level-up"))
    else ("trade", "modal", "#trade_modal", "refresh")

-------------
-- Helpers --
-------------
recombineTeamToPlayers :: Entity Team -> [FullPlayerForTableSansTeam] -> [FullPlayerForTable]
recombineTeamToPlayers teamEntity players =
    map (\(num, player, character, series, widget) -> (num, player, Just teamEntity, character, series, widget)) players

isLeagueMember :: Maybe UserId -> LeagueId -> Handler Bool
isLeagueMember Nothing _ = return False
isLeagueMember justUserId leagueId = do
    teams <- runDB $ selectList [TeamLeagueId ==. leagueId] []
    return $ isJust $ find (\(Entity _ t) -> teamOwnerId t == justUserId) teams

splitStartersAndBench :: [FullPlayerForTable] -> Int -> Int -> [(Text, [FullPlayerForTable], [Int])]
splitStartersAndBench players numberOfStarters rosterSize =
    let (starters, bench) = partition (\(_, Entity _ p, _, _, _, _) -> playerIsStarter p) players
        benchWithNumbers = map (\(n, p, mt, c, s, w) -> (n - numberOfStarters, p, mt, c, s, w)) bench
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

sortByTeam :: [FullPlayerForTable] -> [FullPlayerForTableReqTeam]
sortByTeam players =
    let withTeams = map (\(n, p, mt, c, s, w) -> (n, p, fromJust mt, c, s, w)) players
    in  sortBy (\(_, _, Entity _ t1, _, _, _) (_, _, Entity _ t2, _, _, _) -> teamName t1 `compare` teamName t2) withTeams

