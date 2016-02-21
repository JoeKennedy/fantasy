module Handler.League.Player where

import Import
import Handler.Common        (listByThirdOfFive)
import Handler.League.Layout

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
    leaguePlayers <- getPlayers leagueId
    allPlayers <- playersWithButtons leagueId leaguePlayers
    let (freeAgents, onRosters) = partition (\(_, _, maybeTeam, _, _) -> isNothing maybeTeam) allPlayers
        teamsAndPlayers = listByThirdOfFive $ map (\(n, p, mt, c, w) -> (n, p, fromJust mt, c, w)) onRosters
    leagueLayout leagueId "Players" $(widgetFile "league/players")

getLeaguePlayerR :: LeagueId -> PlayerId -> Handler ()
getLeaguePlayerR leagueId playerId = do
    player <- runDB $ get404 playerId
    redirect $ case playerTeamId player of Just teamId -> LeagueTeamR leagueId teamId
                                           Nothing -> LeaguePlayersR leagueId

postLeaguePlayerStartR :: LeagueId -> PlayerId -> Handler ()
postLeaguePlayerStartR leagueId playerId = do
    player <- runDB $ get404 playerId
    if playerIsStarter player || isNothing (playerTeamId player)
        then error "This player cannot be started or is already started"  
        else do
            Entity _ generalSettings <- runDB $ getBy404 $ UniqueGeneralSettingsLeagueId leagueId
            let teamId = fromJust $ playerTeamId player
            team <- runDB $ get404 teamId
            character <- runDB $ get404 $ playerCharacterId player
            if teamStartersCount team >= generalSettingsNumberOfStarters generalSettings
                then setMessage $ toMarkup $ "You need to bench another player before starting "
                        ++ characterName character
                else do
                    runDB $ update playerId [PlayerIsStarter =. True]
                    runDB $ update teamId [TeamStartersCount +=. 1]
                    setMessage $ toMarkup $ characterName character
                            ++ " is now starting in your lineup"

postLeaguePlayerBenchR :: LeagueId -> PlayerId -> Handler ()
postLeaguePlayerBenchR leagueId playerId = do
    player <- runDB $ get404 playerId
    if not (playerIsStarter player) || isNothing (playerTeamId player)
        then error "This player cannot be bench or is already benched"
        else do
            Entity _ generalSettings <- runDB $ getBy404 $ UniqueGeneralSettingsLeagueId leagueId
            team <- runDB $ get404 $ fromJust $ playerTeamId player
            runDB $ update playerId [PlayerIsStarter =. False]
            runDB $ update (fromJust $ playerTeamId player) [TeamStartersCount -=. 1]
            character <- runDB $ get404 $ playerCharacterId player
            let spotsLeft = generalSettingsNumberOfStarters generalSettings - teamStartersCount team + 1
            setMessage $ toMarkup $ characterName character
                    ++ " is now benched in your lineup and you have "
                    ++ pack (show spotsLeft) ++ " spots for starters"

postLeaguePlayerClaimR :: LeagueId -> PlayerId -> Handler ()
postLeaguePlayerClaimR _ _ = error "Not yet implemented"

postLeaguePlayerTradeR :: LeagueId -> PlayerId -> Handler ()
postLeaguePlayerTradeR _ _ = error "Not yet implemented"

-------------
-- Helpers --
-------------
playersTable :: [(Int, Entity Player, Maybe (Entity Team), Entity Character, Widget)] ->
                PlayersTableType -> Widget
playersTable players playersTableType =
    let colspan = playersTableColumnCount playersTableType
        subTables = case playersTableType of
            Players numberOfStarters rosterSize ->
                splitStartersAndBench players numberOfStarters rosterSize
            _ -> [(packPlayersTableType playersTableType, players, [])]
    in  $(widgetFile "league/players_table")

getPlayers :: LeagueId -> Handler [(Entity Player, Maybe (Entity Team), Entity Character)]
getPlayers leagueId = runDB
    $ E.select
    $ E.from $ \(player `E.InnerJoin` character `E.LeftOuterJoin` team) -> do
        E.on $ E.just (player ^. PlayerTeamId) E.==. E.just (team ?. TeamId)
        E.on $ player ^. PlayerCharacterId E.==. character ^. CharacterId
        E.where_ $ player ^. PlayerLeagueId E.==. E.val leagueId
        E.orderBy [E.asc (character ^. CharacterName)]
        return (player, team, character)

playersWithButtons :: LeagueId -> [(Entity Player, Maybe (Entity Team), Entity Character)] ->
                      Handler [(Int, Entity Player, Maybe (Entity Team), Entity Character, Widget)]
playersWithButtons leagueId players = do
    maybeUserId <- maybeAuthId
    isUserLeagueMember <- isLeagueMember maybeUserId leagueId
    let playersAndButtons = if not isUserLeagueMember
            then map (\(p, mt, c) -> (p, mt, c, [whamlet||])) players
            else map (\(p, mt, c) -> playerWithButton p mt c (fromJust maybeUserId)) players
    return $ zipWith (\n (a,b,c,d) -> (n,a,b,c,d)) [1..] playersAndButtons

playerWithButton :: Entity Player -> Maybe (Entity Team) -> Entity Character -> UserId ->
                    (Entity Player, Maybe (Entity Team), Entity Character, Widget)
playerWithButton player maybeTeam character userId =
    let (action, text, icon) = playerButtonAttributes player maybeTeam userId
    in  (player, maybeTeam, character, $(widgetFile "league/player_action_button"))

playerButtonAttributes :: Entity Player -> Maybe (Entity Team) -> UserId -> (Route App, Text, Text)
playerButtonAttributes (Entity playerId player) Nothing _ =
    (LeaguePlayerClaimR (playerLeagueId player) playerId, "CLAIM", "plus")
playerButtonAttributes (Entity playerId player) (Just (Entity _ team)) userId =
    let leagueId = playerLeagueId player
    in  if isTeamOwner (Just userId) (Just team) then (if playerIsStarter player
            then (LeaguePlayerBenchR leagueId playerId, "BENCH", "chevron-down")
            else (LeaguePlayerStartR leagueId playerId, "START", "chevron-up"))
        else (LeaguePlayerTradeR leagueId playerId, "TRADE", "refresh")

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

