module Handler.League.Player where

import Import
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
type FullPlayer = (Entity Player, Entity PlayerSeason, Maybe (Entity Performance),
                   Maybe (Entity Team), Entity Character, Entity Series)
type FullPlayerForTable = (Int, Entity Player, Entity PlayerSeason,
                           Maybe (Entity Performance), Maybe (Entity Team),
                           Entity Character, Entity Series, Widget)
type FullPlayerWithButton = (Entity Player, Entity PlayerSeason,
                             Maybe (Entity Performance), Maybe (Entity Team),
                             Entity Character, Entity Series, Widget)
type FullPlayerForTableSansTeam = (Int, Entity Player, Entity PlayerSeason,
                                   Maybe (Entity Performance), Entity Character,
                                   Entity Series, Widget)
type FullPlayerForTableReqTeam = (Int, Entity Player, Entity PlayerSeason,
                                  Maybe (Entity Performance), Entity Team,
                                  Entity Character, Entity Series, Widget)

------------
-- Routes --
------------
getLeaguePlayersR :: LeagueId -> Handler Html
getLeaguePlayersR leagueId = do
    league <- runDB $ get404 leagueId
    maybeTeamId <- maybeAuthTeamId leagueId
    Entity seasonId season <- getSelectedSeason leagueId
    Entity _ generalSettings <- runDB $ getBy404 $ UniqueGeneralSettingsSeasonId seasonId
    series <- runDB $ get404 $ seasonSeriesId season
    weekId <- getMostRecentWeekId leagueId seasonId
    leaguePlayers <- getPlayers seasonId weekId
    allPlayers <- playersWithButtons leagueId season leaguePlayers
    currentTeamPlayers <- getTeamPlayers seasonId weekId maybeTeamId
    myPlayers <- playersWithButtons leagueId season currentTeamPlayers
    let (freeAgents, onRosters) = partition (\(_, _, _, _, maybeTeam, _, _, _) -> isNothing maybeTeam) allPlayers
        teamsAndPlayers = groupByFifthOfEight $ sortByTeam onRosters
        numberOfStarters = generalSettingsNumberOfStarters generalSettings
        rosterSize = generalSettingsRosterSize generalSettings
    leagueLayout leagueId "Characters" $(widgetFile "league/players")

getLeaguePlayerR :: LeagueId -> CharacterId -> Handler Html
getLeaguePlayerR leagueId characterId = do
    maybeUser <- maybeAuth
    league <- runDB $ get404 leagueId
    Entity playerId _ <- runDB $ getBy404 $ UniquePlayerLeagueIdCharacterId leagueId characterId
    character <- runDB $ get404 characterId
    blurbs <- runDB $ selectList [BlurbCharacterId ==. characterId] [Desc BlurbId]
    Entity seasonId season <- getSelectedSeason leagueId
    performances <- getPerformancesForPlayer playerId seasonId
    playsByWeek <- mapM (\(p, _, _) -> getPlaysForPerformance p) performances
    let performancesAndPlays = zip performances playsByWeek
    Entity _ playerSeason <- runDB $ getBy404 $ UniquePlayerSeasonPlayerIdSeasonId playerId seasonId
    maybeLastSeasonId <- getLastSeasonId season
    maybeLastPlayerSeason <- case maybeLastSeasonId of
        Nothing -> return Nothing
        Just lastSeasonId -> do
            lastSeasonEntity <- runDB $ getBy404 $ UniquePlayerSeasonPlayerIdSeasonId playerId lastSeasonId
            return $ Just $ entityVal lastSeasonEntity
    -- TODO - uncomment this once season 7 episode 1 is scored
    -- weekId <- getMostRecentWeekId leagueId seasonId
    -- Entity _ performance <- runDB $ getBy404 $ UniquePerformanceWeekIdPlayerId weekId playerId
    maybeTeam <- case playerSeasonTeamId playerSeason of
        Nothing     -> return Nothing
        Just teamId -> do
            team <- runDB $ get404 teamId
            return $ Just $ Entity teamId team
    leagueLayout leagueId "Characters" $(widgetFile "league/player")

postLeaguePlayerStartR :: LeagueId -> CharacterId -> Handler ()
postLeaguePlayerStartR leagueId characterId = do
    Entity playerId player <- runDB $ getBy404 $ UniquePlayerLeagueIdCharacterId leagueId characterId
    seasonId <- getSelectedSeasonId leagueId
    Entity playerSeasonId playerSeason <- runDB $ getBy404 $ UniquePlayerSeasonPlayerIdSeasonId playerId seasonId
    transactionId <- singlePlayerTransaction playerSeason Start
    didAutoFail <- autoFailStartTransaction leagueId transactionId player playerSeason
    if didAutoFail then return () else do
        Entity _ generalSettings <- runDB $ getBy404 $ UniqueGeneralSettingsSeasonId seasonId
        let teamId = fromJust $ playerSeasonTeamId playerSeason
        Entity _ teamSeason <- runDB $ getBy404 $ UniqueTeamSeasonTeamIdSeasonId teamId seasonId
        character <- runDB $ get404 characterId
        if teamSeasonStartersCount teamSeason >= generalSettingsNumberOfStarters generalSettings
            then do
                let message = "You need to bench another player before starting "
                              ++ characterName character
                failTransaction_ transactionId "Can't start player when no slots are available"
                addMessage "danger" $ toMarkup message
            else do
                now <- liftIO getCurrentTime
                userId <- requireAuthId
                runDB $ update playerSeasonId [ PlayerSeasonIsStarter =. True
                                              , PlayerSeasonUpdatedAt =. now
                                              , PlayerSeasonUpdatedBy =. userId ]
                updateTeamSeasonStartersCount_ teamId seasonId now userId
                succeedTransaction transactionId
                let message = characterName character ++ " is now starting in your lineup."
                setMessage $ toMarkup message

postLeaguePlayerBenchR :: LeagueId -> CharacterId -> Handler ()
postLeaguePlayerBenchR leagueId characterId = do
    Entity playerId player <- runDB $ getBy404 $ UniquePlayerLeagueIdCharacterId leagueId characterId
    seasonId <- getSelectedSeasonId leagueId
    Entity playerSeasonId playerSeason <- runDB $ getBy404 $ UniquePlayerSeasonPlayerIdSeasonId playerId seasonId
    transactionId <- singlePlayerTransaction playerSeason Bench
    didAutoFail <- autoFailBenchTransaction leagueId transactionId player playerSeason
    if didAutoFail then return () else do
        now <- liftIO getCurrentTime
        userId <- requireAuthId
        Entity _ generalSettings <- runDB $ getBy404 $ UniqueGeneralSettingsSeasonId seasonId
        runDB $ update playerSeasonId [ PlayerSeasonIsStarter =. False
                                      , PlayerSeasonUpdatedAt =. now
                                      , PlayerSeasonUpdatedBy =. userId ]
        let teamId = fromJust $ playerSeasonTeamId playerSeason
        startersCount <- updateTeamSeasonStartersCount teamId seasonId now userId
        succeedTransaction transactionId
        character <- runDB $ get404 characterId
        let spotsLeft = generalSettingsNumberOfStarters generalSettings - startersCount
            spotOrSpots = if spotsLeft == 1 then "spot" else "spots"
            message = characterName character ++ " is now benched in your lineup and you have "
                      ++ pack (show spotsLeft) ++ " " ++ spotOrSpots ++ " for starters."
        setMessage $ toMarkup message

postLeaguePlayerClaimR :: LeagueId -> CharacterId -> CharacterId -> Handler ()
postLeaguePlayerClaimR leagueId characterIdToAdd characterIdToDrop = do
    Entity playerIdToAdd  playerToAdd  <- runDB $ getBy404 $ UniquePlayerLeagueIdCharacterId leagueId characterIdToAdd
    Entity playerIdToDrop playerToDrop <- runDB $ getBy404 $ UniquePlayerLeagueIdCharacterId leagueId characterIdToDrop
    characterToAdd  <- runDB $ get404 characterIdToAdd
    characterToDrop <- runDB $ get404 characterIdToDrop
    seasonId <- getSelectedSeasonId leagueId
    Entity _ playerToAddSeason <- runDB $ getBy404 $ UniquePlayerSeasonPlayerIdSeasonId playerIdToAdd seasonId
    Entity _ playerToDropSeason <- runDB $ getBy404 $ UniquePlayerSeasonPlayerIdSeasonId playerIdToDrop seasonId
    transactionId <- twoPlayerTransaction playerToAddSeason playerToDropSeason Claim

    didAutoFail <- autoFailClaimTransaction leagueId transactionId playerToAdd playerToAddSeason playerToDrop playerToDropSeason
    if didAutoFail then return () else do
        transaction <- runDB $ get404 transactionId
        Entity _ generalSettings <- runDB $ getBy404 $ UniqueGeneralSettingsSeasonId seasonId
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
    seasonId <- getSelectedSeasonId leagueId
    Entity _ playerToTakeSeason <- runDB $ getBy404 $ UniquePlayerSeasonPlayerIdSeasonId playerIdToTake seasonId
    Entity _ playerToGiveSeason <- runDB $ getBy404 $ UniquePlayerSeasonPlayerIdSeasonId playerIdToGive seasonId
    transactionId <- twoPlayerTransaction playerToTakeSeason playerToGiveSeason Trade

    didAutoFail <- autoFailTradeTransaction leagueId transactionId playerToTake playerToTakeSeason playerToGive playerToGiveSeason
    if didAutoFail then return () else do
        otherTeam <- runDB $ get404 $ fromJust $ playerSeasonTeamId playerToTakeSeason
        characterToTake <- runDB $ get404 characterIdToTake
        characterToGive <- runDB $ get404 characterIdToGive
        setMessage $ toMarkup $ "Trade to give " ++ characterName characterToGive ++
            " to House " ++ teamName otherTeam ++ " in exchange for " ++
            characterName characterToTake ++ " has been submitted. It is up to House " ++
            teamName otherTeam ++ " to accept or deny the trade."


-------------
-- Widgets --
-------------
playersTable :: [FullPlayerForTable] -> PlayersTableType -> Series -> Widget
playersTable players playersTableType series =
    let colspan = playersTableColumnCount playersTableType
        subTables = case splitPlayersTable playersTableType of
            Nothing -> [(packPlayersTableType playersTableType, players, [])]
            Just (numberOfStarters, rosterSize) ->
                splitStartersAndBench players numberOfStarters rosterSize
    in  $(widgetFile "league/players_table")

playersModal :: [FullPlayerForTable] -> PlayersTableType -> Series -> TransactionType -> Widget
playersModal players playersTableType series modalType =
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
getPlayers :: SeasonId -> WeekId -> Handler [FullPlayer]
getPlayers seasonId weekId = runDB
    $ E.select
    $ E.from $ \(player `E.InnerJoin` playerSeason `E.LeftOuterJoin` performance `E.InnerJoin` character `E.LeftOuterJoin` team `E.InnerJoin` series) -> do
        E.on $ character ^. CharacterRookieSeriesId E.==. series ^. SeriesId
        E.on $ E.just (playerSeason ^. PlayerSeasonTeamId) E.==. E.just (team ?. TeamId)
        E.on $ player ^. PlayerCharacterId E.==. character ^. CharacterId
        E.on $ performance ?. PerformancePlayerId E.==. E.just (player ^. PlayerId)
            E.&&. performance ?. PerformanceWeekId E.==. E.just (E.val weekId)
        E.on $ playerSeason ^. PlayerSeasonPlayerId E.==. player ^. PlayerId
        E.where_ $ playerSeason ^. PlayerSeasonSeasonId E.==. E.val seasonId
            E.&&. player ^. PlayerIsPlayable E.==. E.val True
        E.orderBy [E.asc (character ^. CharacterName)]
        return (player, playerSeason, performance, team, character, series)

getTeamPlayers :: SeasonId -> WeekId -> Maybe TeamId -> Handler [FullPlayer]
getTeamPlayers _ _ Nothing = return []
getTeamPlayers seasonId weekId (Just teamId) = runDB
    $ E.select
    $ E.from $ \(player `E.InnerJoin` playerSeason `E.LeftOuterJoin` performance `E.InnerJoin` character `E.LeftOuterJoin` team `E.InnerJoin` series) -> do
        E.on $ character ^. CharacterRookieSeriesId E.==. series ^. SeriesId
        E.on $ E.just (playerSeason ^. PlayerSeasonTeamId) E.==. E.just (team ?. TeamId)
        E.on $ player ^. PlayerCharacterId E.==. character ^. CharacterId
        E.on $ performance ?. PerformancePlayerId E.==. E.just (player ^. PlayerId)
            E.&&. performance ?. PerformanceWeekId E.==. E.just (E.val weekId)
        E.on $ playerSeason ^. PlayerSeasonPlayerId E.==. player ^. PlayerId
        E.where_ $ playerSeason ^. PlayerSeasonTeamId E.==. E.just (E.val teamId)
            E.&&. playerSeason ^. PlayerSeasonSeasonId E.==. E.val seasonId
        E.orderBy [ E.desc (playerSeason ^. PlayerSeasonIsStarter)
                  , E.asc (character ^. CharacterName)
                  ]
        return (player, playerSeason, performance, team, character, series)


--------------------------
-- Player Action Button --
--------------------------
playersWithButtons :: LeagueId -> Season -> [FullPlayer] -> Handler [FullPlayerForTable]
playersWithButtons leagueId season players = do
    maybeUserId <- maybeAuthId
    isUserLeagueMember <- isLeagueMember maybeUserId leagueId
    let transactionsPossible = seasonIsDraftComplete season && (not $ seasonIsSeasonComplete season)
        isAfterTradeDeadline = seasonIsAfterTradeDeadline season
        playersAndButtons = if isUserLeagueMember && transactionsPossible
            then map (playerWithButton isAfterTradeDeadline $ fromJust maybeUserId) players
            else map playerWithNoButton players
    return $ rank7 playersAndButtons

playerWithButton :: Bool -> UserId -> FullPlayer -> FullPlayerWithButton
playerWithButton isAfterTradeDeadline userId ((Entity playerId player), playerSeason, performance, maybeTeam, character, series) =
    let (text, dataToggle, dataTarget, icon) = playerButtonAttributes playerSeason maybeTeam userId
        buttonId = text ++ "-" ++ toPathPiece (playerLeagueId player) ++ "-" ++ toPathPiece (playerCharacterId player)
        name = characterName $ entityVal character
        isPostDeadlineTrade = text == "trade" && isAfterTradeDeadline
    in  ((Entity playerId player), playerSeason, performance, maybeTeam, character, series, $(widgetFile "league/player_action_button"))

playerButtonAttributes :: Entity PlayerSeason -> Maybe (Entity Team) -> UserId -> (Text, Text, Text, Text)
playerButtonAttributes _ Nothing _ = ("claim", "modal", "#claim_modal", "plus")
playerButtonAttributes (Entity _ playerSeason) (Just (Entity _ team)) userId =
    if isTeamOwner (Just userId) (Just team) then (if playerSeasonIsStarter playerSeason
        then ("bench", "", "", "level-down")
        else ("start", "", "", "level-up"))
    else ("trade", "modal", "#trade_modal", "refresh")

playerWithNoButton :: (a, b, c, d, e, f) -> (a, b, c, d, e, f, Widget)
playerWithNoButton (u, v, w, x, y, z) = (u, v, w, x, y, z, [whamlet||])


-------------
-- Helpers --
-------------
recombineTeamToPlayers :: Entity Team -> [FullPlayerForTableSansTeam] -> [FullPlayerForTable]
recombineTeamToPlayers teamEntity players =
    map (\(num, player, playerSeason, performance, character, series, widget) -> (num, player, playerSeason, performance, Just teamEntity, character, series, widget)) players

isLeagueMember :: Maybe UserId -> LeagueId -> Handler Bool
isLeagueMember Nothing _ = return False
isLeagueMember justUserId leagueId = do
    teams <- runDB $ selectList [TeamLeagueId ==. leagueId] []
    return $ isJust $ find (\(Entity _ t) -> teamOwnerId t == justUserId) teams

splitStartersAndBench :: [FullPlayerForTable] -> Int -> Int -> [(Text, [FullPlayerForTable], [Int])]
splitStartersAndBench players numberOfStarters rosterSize =
    let (starters, bench) = partition (\(_, _, Entity _ ps, _, _, _, _, _) -> playerSeasonIsStarter ps) players
        benchWithNumbers = map (\(n, p, ps, pe, mt, c, s, w) -> (n - numberOfStarters, p, ps, pe, mt, c, s, w)) bench
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
    let withTeams = map (\(n, p, ps, pe, mt, c, s, w) -> (n, p, ps, pe, fromJust mt, c, s, w)) players
    in  sortBy (\(_, _, _, _, Entity _ t1, _, _, _) (_, _, _, _, Entity _ t2, _, _, _) -> teamName t1 `compare` teamName t2) withTeams

