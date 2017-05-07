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

getLeaguePlayerR :: LeagueId -> CharacterId -> Handler Html
getLeaguePlayerR leagueId characterId = do
    maybeUser <- maybeAuth
    league <- runDB $ get404 leagueId
    Entity playerId player <- runDB $ getBy404 $ UniquePlayerLeagueIdCharacterId leagueId characterId
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

postLeaguePlayerStartR :: LeagueId -> CharacterId -> Handler ()
postLeaguePlayerStartR leagueId characterId = do
    Entity playerId player <- runDB $ getBy404 $ UniquePlayerLeagueIdCharacterId leagueId characterId
    transactionId <- singlePlayerTransaction (Entity playerId player) Start
    didAutoFail <- autoFailStartTransaction leagueId transactionId player
    if didAutoFail then return () else do
        Entity _ generalSettings <- runDB $ getBy404 $ UniqueGeneralSettingsLeagueId leagueId
        let teamId = fromJust $ playerTeamId player
        team <- runDB $ get404 teamId
        character <- runDB $ get404 characterId
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

postLeaguePlayerBenchR :: LeagueId -> CharacterId -> Handler ()
postLeaguePlayerBenchR leagueId characterId = do
    Entity playerId player <- runDB $ getBy404 $ UniquePlayerLeagueIdCharacterId leagueId characterId
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
        character <- runDB $ get404 characterId
        team <- runDB $ get404 teamId
        let spotsLeft = generalSettingsNumberOfStarters generalSettings - teamStartersCount team + 1
            message = characterName character ++ " is now benched in your lineup and you have "
                      ++ pack (show spotsLeft) ++ " spots for starters."
        succeedTransaction transactionId
        setMessage $ toMarkup message

postLeaguePlayerClaimR :: LeagueId -> CharacterId -> CharacterId -> Handler ()
postLeaguePlayerClaimR leagueId characterIdToAdd characterIdToDrop = do
    Entity playerIdToAdd  playerToAdd  <- runDB $ getBy404 $ UniquePlayerLeagueIdCharacterId leagueId characterIdToAdd
    Entity playerIdToDrop playerToDrop <- runDB $ getBy404 $ UniquePlayerLeagueIdCharacterId leagueId characterIdToDrop
    characterToAdd  <- runDB $ get404 characterIdToAdd
    characterToDrop <- runDB $ get404 characterIdToDrop
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

postLeaguePlayerTradeR :: LeagueId -> CharacterId -> CharacterId -> Handler ()
postLeaguePlayerTradeR leagueId characterIdToTake characterIdToGive = do
    Entity playerIdToTake playerToTake <- runDB $ getBy404 $ UniquePlayerLeagueIdCharacterId leagueId characterIdToTake
    Entity playerIdToGive playerToGive <- runDB $ getBy404 $ UniquePlayerLeagueIdCharacterId leagueId characterIdToGive
    transactionId <- twoPlayerTransaction (Entity playerIdToTake playerToTake)
                     (Entity playerIdToGive playerToGive) Trade
    didAutoFail <- autoFailTradeTransaction leagueId transactionId playerToTake playerToGive
    if didAutoFail then return () else do
        otherTeam <- runDB $ get404 $ fromJust $ playerTeamId playerToTake
        characterToTake <- runDB $ get404 characterIdToTake
        characterToGive <- runDB $ get404 characterIdToGive
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

blurbPanel :: Maybe (Entity User) -> Entity Blurb -> Widget
blurbPanel maybeUser (Entity blurbId blurb) = $(widgetFile "blurb_panel")

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
    let transactionsPossible = leagueIsDraftComplete league && (not $ leagueIsSeasonComplete league)
        isAfterTradeDeadline = leagueIsAfterTradeDeadline league
        playersAndButtons = if isUserLeagueMember && transactionsPossible
            then map (playerWithButton isAfterTradeDeadline $ fromJust maybeUserId) players
            else map playerWithNoButton players
    return $ zipWith (\n (a,b,c,d,e) -> (n,a,b,c,d,e)) [1..] playersAndButtons

playerWithButton :: Bool -> UserId -> FullPlayer -> FullPlayerWithButton
playerWithButton isAfterTradeDeadline userId ((Entity playerId player), maybeTeam, character, series) =
    let (text, dataToggle, dataTarget, icon) = playerButtonAttributes (Entity playerId player) maybeTeam userId
        buttonId = text ++ "-" ++ toPathPiece (playerLeagueId player) ++ "-" ++ toPathPiece (playerCharacterId player)
        name = characterName $ extractValue character
        isPostDeadlineTrade = text == "trade" && isAfterTradeDeadline
    in  (Entity playerId player, maybeTeam, character, series, $(widgetFile "league/player_action_button"))

playerButtonAttributes :: Entity Player -> Maybe (Entity Team) -> UserId -> (Text, Text, Text, Text)
playerButtonAttributes _ Nothing _ = ("claim", "modal", "#claim_modal", "plus")
playerButtonAttributes (Entity _ player) (Just (Entity _ team)) userId =
    if isTeamOwner (Just userId) (Just team) then (if playerIsStarter player
        then ("bench", "", "", "level-down")
        else ("start", "", "", "level-up"))
    else ("trade", "modal", "#trade_modal", "refresh")

playerWithNoButton :: (a, b, c, d) -> (a, b, c, d, Widget)
playerWithNoButton (w, x, y, z) = (w, x, y, z, [whamlet||])

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

