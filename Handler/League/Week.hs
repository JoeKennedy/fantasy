module Handler.League.Week where

import Import

import Handler.League.Layout

import qualified Database.Esqueleto as E
import           Database.Esqueleto ((^.), (?.))

-----------
-- Types --
-----------
type FullPerformance = (Entity Performance, Entity Week, Entity Player, Maybe (Entity Team), Entity Character)
type FullGame = (Entity Game, Entity Week, Entity Team)
type FullPlay = ( Entity Play, Entity Week, Entity Event, Entity Player
                , Entity Character, Maybe (Entity Player), Maybe (Entity Character))

------------
-- Routes --
------------
getLeagueResultsR :: LeagueId -> Handler Html
getLeagueResultsR leagueId = do
    seasonId <- getSelectedSeasonId leagueId
    teams <- getTeamsOrderBy seasonId False TeamSeasonRegularSeasonPoints
    leagueResultsLayout leagueId "Standings" $(widgetFile "league/results")

getLeaguePlayoffsR :: LeagueId -> Handler Html
getLeaguePlayoffsR leagueId = do
    (playoffTs, consolationTs) <- getPlayoffTeams leagueId
    let groupedTeams = [(Playoff, rank2 playoffTs), (Consolation, rank2 consolationTs)]
    leagueResultsLayout leagueId "Playoffs" $(widgetFile "league/playoffs")

getLeagueResultsWeekR :: LeagueId -> Int -> Handler Html
getLeagueResultsWeekR leagueId weekNo = do
    seasonId <- getSelectedSeasonId leagueId
    Entity weekId week <- runDB $ getBy404 $ UniqueWeekSeasonIdNumber seasonId weekNo
    games <- getGamesForWeek weekId
    performances <- getPerformancesForWeek weekId
    plays <- getPlaysForWeek weekId
    let activePill = "Week " ++ pack (show weekNo)
    leagueResultsLayout leagueId activePill $(widgetFile "league/week_results")


-------------
-- Queries --
-------------
getGamesForWeek :: WeekId -> Handler [(Entity Game, Entity Team)]
getGamesForWeek weekId = runDB
    $ E.select
    $ E.from $ \(game `E.InnerJoin` team) -> do
        E.on $ game ^. GameTeamId E.==. team ^. TeamId
        E.where_ $ game ^. GameWeekId E.==. E.val weekId
        E.orderBy [E.desc (game ^. GamePoints)]
        return (game, team)

-- returns all performances of players that appeared in the episode
-- TODO - add a playsCount column to performances and use that
getPerformancesForWeek :: WeekId -> Handler [FullPerformance]
getPerformancesForWeek weekId = runDB
    $ E.select
    $ E.from $ \(performance `E.InnerJoin` week `E.InnerJoin` player `E.LeftOuterJoin` team `E.InnerJoin` character `E.InnerJoin` play) -> do
        E.on $ play ^. PlayPlayerId E.==. performance ^. PerformancePlayerId
            E.&&. play ^. PlayAction E.==. E.val Appear
            E.&&. play ^. PlayWeekId E.==. E.val weekId
        E.on $ player ^. PlayerCharacterId E.==. character ^. CharacterId
        E.on $ E.just (performance ^. PerformanceTeamId) E.==. E.just (team ?. TeamId)
        E.on $ performance ^. PerformancePlayerId E.==. player ^. PlayerId
            E.&&. player ^. PlayerIsPlayable E.==. E.val True
        E.on $ performance ^. PerformanceWeekId E.==. week ^. WeekId
        E.where_ $ performance ^. PerformanceWeekId E.==. E.val weekId
        E.orderBy [E.desc (performance ^. PerformancePoints), E.asc (character ^. CharacterName)]
        return (performance, week, player, team, character)

getPerformancesForGame :: Entity Game -> Handler [FullPerformance]
getPerformancesForGame (Entity _ game) = runDB
    $ E.select
    $ E.from $ \(performance `E.InnerJoin` week `E.InnerJoin` player `E.LeftOuterJoin` team `E.InnerJoin` character) -> do
        E.on $ player ^. PlayerCharacterId E.==. character ^. CharacterId
        E.on $ E.just (performance ^. PerformanceTeamId) E.==. E.just (team ?. TeamId)
        E.on $ performance ^. PerformancePlayerId E.==. player ^. PlayerId
        E.on $ performance ^. PerformanceWeekId E.==. week ^. WeekId
        E.where_ $ performance ^. PerformanceWeekId E.==. E.val (gameWeekId game)
             E.&&. performance ^. PerformanceTeamId E.==. E.just (E.val $ gameTeamId game)
        E.orderBy [E.desc (performance ^. PerformanceIsStarter), E.asc (character ^. CharacterName)]
        return (performance, week, player, team, character)

getPerformancesForPlayer :: PlayerId -> SeasonId -> Handler [(Entity Performance, Entity Week, Maybe (Entity Team))]
getPerformancesForPlayer playerId seasonId = runDB
    $ E.select
    $ E.from $ \(performance `E.InnerJoin` week `E.LeftOuterJoin` team) -> do
        E.on $ E.just (performance ^. PerformanceTeamId) E.==. E.just (team ?. TeamId)
        E.on $ performance ^. PerformanceWeekId E.==. week ^. WeekId
        E.where_ $ performance ^. PerformancePlayerId E.==. E.val playerId
            E.&&. week ^. WeekSeasonId E.==. E.val seasonId
        E.orderBy [E.desc (week ^. WeekNumber)]
        return (performance, week, team)

getPlaysForPerformance :: Entity Performance -> Handler [FullPlay]
getPlaysForPerformance (Entity _ performance) = runDB
    $ E.select
    $ E.from $ \(play `E.InnerJoin` week `E.InnerJoin` event `E.InnerJoin` player `E.InnerJoin` character `E.LeftOuterJoin` recPlayer `E.LeftOuterJoin` recCharacter) -> do
        E.on $ E.just (recPlayer ?. PlayerCharacterId) E.==. E.just (recCharacter ?. CharacterId)
        E.on $ E.just (play ^. PlayReceivingPlayerId) E.==. E.just (recPlayer ?. PlayerId)
        E.on $ player ^. PlayerCharacterId E.==. character ^. CharacterId
        E.on $ play ^. PlayPlayerId E.==. player ^. PlayerId
        E.on $ play ^. PlayEventId E.==. event ^. EventId
        E.on $ play ^. PlayWeekId E.==. week ^. WeekId
        E.where_ $ play ^. PlayWeekId E.==. E.val (performanceWeekId performance)
            E.&&. (play ^. PlayPlayerId E.==. E.val (performancePlayerId performance)
                  E.||. play ^. PlayReceivingPlayerId E.==. E.just (E.val $ performancePlayerId performance))
        E.orderBy [ E.asc (event ^. EventTimeInEpisode)
                  , E.asc (event ^. EventAction)
                  , E.asc (event ^. EventId)
                  ]
        return (play, week, event, player, character, recPlayer, recCharacter)

getPlaysForWeek :: WeekId -> Handler [FullPlay]
getPlaysForWeek weekId = runDB
    $ E.select
    $ E.from $ \(play `E.InnerJoin` week `E.InnerJoin` event `E.InnerJoin` player `E.InnerJoin` character `E.LeftOuterJoin` recPlayer `E.LeftOuterJoin` recCharacter) -> do
        E.on $ E.just (recPlayer ?. PlayerCharacterId) E.==. E.just (recCharacter ?. CharacterId)
        E.on $ E.just (play ^. PlayReceivingPlayerId) E.==. E.just (recPlayer ?. PlayerId)
        E.on $ player ^. PlayerCharacterId E.==. character ^. CharacterId
        E.on $ play ^. PlayPlayerId E.==. player ^. PlayerId
        E.on $ play ^. PlayEventId E.==. event ^. EventId
        E.on $ play ^. PlayWeekId E.==. week ^. WeekId
        E.where_ $ play ^. PlayWeekId E.==. E.val weekId
        E.orderBy [ E.asc (event ^. EventTimeInEpisode)
                  , E.asc (event ^. EventAction)
                  , E.asc (event ^. EventId)
                  ]
        return (play, week, event, player, character, recPlayer, recCharacter)


-------------
-- Layouts --
-------------
leagueResultsLayout :: LeagueId -> Text -> Widget -> Handler Html
leagueResultsLayout leagueId activePill widget = do
    league <- runDB $ get404 leagueId
    Entity seasonId season <- getSelectedSeason leagueId
    weeks  <- runDB $ selectList [WeekSeasonId ==. seasonId] [Asc WeekNumber]
    leagueLayout leagueId "Results" $(widgetFile "layouts/results")


-------------
-- Helpers --
-------------
getPlayoffTeams :: LeagueId -> Handler ([(Entity Team, Entity TeamSeason)], [(Entity Team, Entity TeamSeason)])
getPlayoffTeams leagueId = do
    seasonId <- getSelectedSeasonId leagueId
    teams <- getTeamsOrderBy seasonId False TeamSeasonPostSeasonPoints
    return $ partition (\(_, Entity _ ts) -> teamSeasonPostSeasonStatus ts == Playoff) teams
