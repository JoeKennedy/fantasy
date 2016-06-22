module Handler.League.Week where

import Import

import Handler.Common             (extractValue)
import Handler.League.Layout
import Handler.League.Transaction (cancelAllTrades)

import qualified Database.Esqueleto as E
import           Database.Esqueleto ((^.), (?.))
import           Data.Maybe         (fromJust)

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
    teams <- runDB $ selectList [TeamLeagueId ==. leagueId]
                                [Desc TeamPointsThisRegularSeason, Desc TeamDraftOrder]
    leagueResultsLayout leagueId "Standings" $(widgetFile "league/results")

getLeaguePlayoffsR :: LeagueId -> Handler Html
getLeaguePlayoffsR leagueId = do
    (playoffTs, consolationTs) <- getPlayoffTeams leagueId
    let groupedTeams = [(Playoff, rank playoffTs), (Consolation, rank consolationTs)]
    leagueResultsLayout leagueId "Playoffs" $(widgetFile "league/playoffs")

getLeagueResultsWeekR :: LeagueId -> Int -> Handler Html
getLeagueResultsWeekR leagueId weekNo = do
    Entity weekId week <- runDB $ getBy404 $ UniqueWeekLeagueIdWeekNumber leagueId weekNo
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

getGamesForTeam :: TeamId -> Handler [(Entity Game, Entity Week)]
getGamesForTeam teamId = runDB
    $ E.select
    $ E.from $ \(game `E.InnerJoin` week) -> do
        E.on $ game ^. GameWeekId E.==. week ^. WeekId
        E.where_ $ game ^. GameTeamId E.==. E.val teamId
        E.orderBy [E.asc (week ^. WeekNumber)]
        return (game, week)

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
        E.orderBy [E.desc (player ^. PlayerIsStarter), E.asc (character ^. CharacterName)]
        return (performance, week, player, team, character)

getPerformancesForPlayer :: PlayerId -> Handler [(Entity Performance, Entity Week, Maybe (Entity Team))]
getPerformancesForPlayer playerId = runDB
    $ E.select
    $ E.from $ \(performance `E.InnerJoin` week `E.LeftOuterJoin` team) -> do
        E.on $ E.just (performance ^. PerformanceTeamId) E.==. E.just (team ?. TeamId)
        E.on $ performance ^. PerformanceWeekId E.==. week ^. WeekId
        E.where_ $ performance ^. PerformancePlayerId E.==. E.val playerId
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
    weeks  <- runDB $ selectList [WeekLeagueId ==. leagueId] [Asc WeekNumber]
    leagueLayout leagueId "Results" $(widgetFile "layouts/results")


--------------
-- Creators --
--------------
createWeekData :: Entity Episode -> LeagueId -> Handler WeekId
createWeekData (Entity episodeId episode) leagueId = do
    -- create week if week not already created for episode
    alreadyCreatedWeek <- runDB $ getBy $ UniqueWeekLeagueIdEpisodeId leagueId episodeId
    case alreadyCreatedWeek of
        Just (Entity weekId _) -> return weekId
        Nothing -> do
            let weekNo = episodeNumber episode
            weekId <- createWeek leagueId episodeId weekNo
            -- then, for the week:
            teamIds <- runDB $ selectKeysList [TeamLeagueId ==. leagueId] []
            -- create game for each team in league
            mapM_ (createGame leagueId weekId) teamIds
            -- create performance for each player in league
            players <- runDB $ selectList [PlayerLeagueId ==. leagueId] []
            mapM_ (createPerformance leagueId weekId) players
            -- Cancel all trades if passed the trade deadline
            league <- runDB $ get404 leagueId
            determineIfTradeDeadlineHasPassed weekNo $ Entity leagueId league

            return weekId

createWeekData_ :: Entity Episode -> LeagueId -> Handler ()
createWeekData_ episodeEntity leagueId = do
    _ <- createWeekData episodeEntity leagueId
    return ()

createWeek :: LeagueId -> EpisodeId -> Int -> Handler WeekId
createWeek leagueId episodeId episodeNo = runDB $ do
    now <- liftIO getCurrentTime
    league <- get404 leagueId
    insert $ Week { weekLeagueId     = leagueId
                  , weekEpisodeId    = episodeId
                  , weekNumber       = episodeNo
                  , weekIsScored     = False
                  , weekIsPostSeason = leagueIsInPostSeason league
                  , weekCreatedAt    = now
                  , weekUpdatedAt    = now
                  }

createGame :: LeagueId -> WeekId -> TeamId -> Handler ()
createGame leagueId weekId teamId = do
    now <- liftIO getCurrentTime
    runDB $ insert_ $ Game { gameLeagueId  = leagueId
                           , gameWeekId    = weekId
                           , gameTeamId    = teamId
                           , gamePoints    = 0
                           , gameCreatedAt = now
                           , gameUpdatedAt = now
                           }

createPerformance :: LeagueId -> WeekId -> Entity Player -> Handler ()
createPerformance leagueId weekId (Entity playerId player) = do
    now <- liftIO getCurrentTime
    maybePerformance <- runDB $ getBy $ UniquePerformanceWeekIdPlayerId weekId playerId
    (cumulativePoints, cappedCumulativePoints) <- calculateCumulativePoints player
    case maybePerformance of
        Just _ -> return ()
        Nothing -> runDB $ insert_ $ Performance
            { performanceLeagueId  = leagueId
            , performanceWeekId    = weekId
            , performancePlayerId  = playerId
            , performanceTeamId    = playerTeamId player
            , performanceIsStarter = playerIsStarter player
            , performancePoints    = 0
            , performanceCumulativePoints = cumulativePoints
            , performanceCappedCumulativePoints = cappedCumulativePoints
            , performanceCreatedAt = now
            , performanceUpdatedAt = now
            }

-------------
-- Helpers --
-------------
calculateCumulativePoints :: Player -> Handler (Rational, Rational)
calculateCumulativePoints player = do
    character <- runDB $ get404 $ playerCharacterId player
    let pointsThisSeason = playerPointsThisSeason player
        pointsLastSeason = toRational $ characterPointsLastSeason character
        cumulativePoints = pointsThisSeason + pointsLastSeason
        cappedCumulative = max 0 $ pointsThisSeason + pointsLastSeason
    return (cumulativePoints, cappedCumulative)

updateCumulativePoints :: Entity Performance -> Handler ()
updateCumulativePoints (Entity performanceId performance) = do
    player <- runDB $ get404 $ performancePlayerId performance
    (cumulativePoints, cappedCumulativePoints) <- calculateCumulativePoints player
    now <- liftIO getCurrentTime
    runDB $ update performanceId [ PerformanceCumulativePoints =. cumulativePoints
                                 , PerformanceCappedCumulativePoints =. cappedCumulativePoints
                                 , PerformanceUpdatedAt =. now
                                 ]

getPlayoffTeams :: LeagueId -> Handler ([Entity Team], [Entity Team])
getPlayoffTeams leagueId = do
    teams <- runDB $ selectList [TeamLeagueId ==. leagueId] [Desc TeamPointsThisPostSeason]
    return $ partition (\(Entity _ t) -> teamPostSeasonStatus t == Playoff) teams

beforeTradeDeadline :: LeagueId -> Handler Bool
beforeTradeDeadline leagueId = do
    maybeWeek <- runDB $ selectFirst [WeekLeagueId ==. leagueId] [Desc WeekNumber]
    let currentWeek = fromMaybe 0 $ map (weekNumber . extractValue) maybeWeek
    weekNumberBeforeTradeDeadline leagueId currentWeek

weekNumberBeforeTradeDeadline :: LeagueId -> Int -> Handler Bool
weekNumberBeforeTradeDeadline leagueId weekNo = do
    Entity _ generalSettings <- runDB $ getBy404 $ UniqueGeneralSettingsLeagueId leagueId
    return $ weekNo < generalSettingsTradeDeadlineWeek generalSettings

determineIfTradeDeadlineHasPassed :: Int -> Entity League -> Handler ()
determineIfTradeDeadlineHasPassed weekNo (Entity leagueId league) =
    if leagueIsAfterTradeDeadline league then return () else do
        isBeforeTradeDeadline <- weekNumberBeforeTradeDeadline leagueId weekNo
        if isBeforeTradeDeadline then return () else do
            maybeAdmin <- runDB $ selectFirst [UserIsAdmin ==. True] [Asc UserId]
            let Entity adminUserId _ = fromJust maybeAdmin
            now <- liftIO getCurrentTime
            runDB $ update leagueId [ LeagueIsAfterTradeDeadline =. True
                                    , LeagueUpdatedBy =. adminUserId
                                    , LeagueUpdatedAt =. now
                                    ]
            cancelAllTrades adminUserId leagueId
