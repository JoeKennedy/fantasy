module Handler.League.Transaction where

import Import

import Handler.League.Layout
import Handler.Common        (groupByFirstOfFour, groupByFirstOfSix,
                              intToOrdinal, quintupleToTuple)

import qualified Database.Esqueleto as E
import           Database.Esqueleto ((^.), (?.))
import qualified Data.List          as List (cycle)
import           Data.Maybe         (fromJust)
import           Text.Blaze         (toMarkup)

-----------
-- Types --
-----------
data DraftPick = DraftPick
    { draftPickNumber   :: Int
    , draftPickTeamId   :: TeamId
    , draftPickPlayerId :: PlayerId
    } deriving Show

type NewTeamIsTransactionTeam = Bool
type ShowTeamAfterCharacter = Bool
type FullTransaction = (Entity Transaction, Entity Team, Entity TransactionPlayer,
                        Entity Player, Entity Character, Maybe (Entity Team))
type FullTransactionPlayer = (Entity TransactionPlayer, Entity Player,
                              Entity Character, Maybe (Entity Team))

-----------
-- Forms --
-----------
draftForm :: DraftSettings -> GeneralSettings -> [Entity Team] -> [(Text, PlayerId)] -> Form [DraftPick]
draftForm draftSettings generalSettings teams playersForSelect extra = do
    let rosterSize = generalSettingsRosterSize generalSettings
        teamsInOrder = case draftSettingsDraftOrder draftSettings of Linear -> teams
                                                                     Snake  -> teams ++ reverse teams
        teamSlots = take (rosterSize * length teams) $ List.cycle teamsInOrder
        numbers = [1..length teamSlots]
        rounds = map (\number -> ((number - 1) `div` length teams) + 1) numbers
    playerIdFields <- forM numbers (\num ->
        mreq (selectFieldList playersForSelect) (inputSmHidden $ toPathPiece num) Nothing)
    let forms = zip4 rounds numbers teamSlots playerIdFields
        groupedForms = groupByFirstOfFour forms
        draftResult = for forms (\(_, number, Entity teamId _, playerIdField) ->
            DraftPick <$> pure number <*> pure teamId <*> fst playerIdField)
    return (draftResult, $(widgetFile "league/draft_form"))

------------
-- Routes --
------------
getLeagueDraftR :: LeagueId -> Int -> Handler Html
getLeagueDraftR leagueId _year = do
    league <- runDB $ get404 leagueId
    teams <- runDB $ selectList [TeamLeagueId ==. leagueId] [Asc TeamDraftOrder]
    if foldr (\(Entity _ t) acc -> teamIsConfirmed t && acc) True teams
        then do
            Entity _ generalSettings <- runDB $ getBy404 $ UniqueGeneralSettingsLeagueId leagueId
            Entity _ draftSettings <- runDB $ getBy404 $ UniqueDraftSettingsLeagueId leagueId
            playersAndCharacters <- getPlayersAndCharacters leagueId
            let playersForSelect =
                    map (\(Entity pid _, Entity _ c) -> (characterName c, pid)) playersAndCharacters
            (widget, enctype) <- generateFormPost $ draftForm draftSettings generalSettings teams playersForSelect
            leagueLayout leagueId "Transactions" $(widgetFile "league/draft")
        else leagueLayout leagueId "Transactions" $(widgetFile "league/no_draft")

postLeagueDraftR :: LeagueId -> Int -> Handler Html
postLeagueDraftR leagueId _year = do
    userId <- requireAuthId
    league <- runDB $ get404 leagueId
    Entity _ generalSettings <- runDB $ getBy404 $ UniqueGeneralSettingsLeagueId leagueId
    Entity _ draftSettings <- runDB $ getBy404 $ UniqueDraftSettingsLeagueId leagueId
    teams <- runDB $ selectList [TeamLeagueId ==. leagueId] [Asc TeamDraftOrder]
    playersAndCharacters <- getPlayersAndCharacters leagueId
    let playersForSelect =
            map (\(Entity pid _, Entity _ c) -> (characterName c, pid)) playersAndCharacters
    ((result, widget), enctype) <- runFormPost $ draftForm draftSettings generalSettings teams playersForSelect
    case result of
        FormSuccess draftPicks -> do
            now <- liftIO getCurrentTime
            forM_ draftPicks (insertDraftPick leagueId userId)
            runDB $ update leagueId [LeagueIsDraftComplete =. True, LeagueUpdatedBy =. userId,
                                     LeagueUpdatedAt =. now, LeagueDraftCompletedAt =. Just now]
            setMessage "Successfully completed your draft! Be sure to let the other members of your league know."
            redirect $ LeagueTransactionsR leagueId
        _ -> leagueLayout leagueId "Draft" $(widgetFile "league/draft")

getLeagueTransactionsR :: LeagueId -> Handler Html
getLeagueTransactionsR leagueId = do
    league <- runDB $ get404 leagueId
    transactions <- getSuccessfulTransactions leagueId Nothing Nothing
    tradeProposals <- getRequestedTransactions leagueId Nothing Trade
    draftTransactions <- getSuccessfulTransactions leagueId Nothing $ Just Draft
    leagueLayout leagueId "Transactions" $(widgetFile "league/transactions")

postLeagueAcceptTradeR :: LeagueId -> TransactionId -> Handler ()
postLeagueAcceptTradeR _ transactionId = do
    userId <- requireAuthId
    processMultiPlayerTransaction userId transactionId
    setMessage $ toMarkup ("Trade accepted!" :: Text)

postLeagueDeclineTradeR :: LeagueId -> TransactionId -> Handler ()
postLeagueDeclineTradeR _ transactionId = do
    failTransaction_ transactionId "Receiving team owner declined trade"
    setMessage $ toMarkup ("Trade declined!" :: Text)

postLeagueCancelTransactionR :: LeagueId -> TransactionId -> Handler ()
postLeagueCancelTransactionR _ transactionId = do
    transaction <- runDB $ get404 transactionId
    let typeStr = toPathPiece $ transactionType transaction
    failTransaction_ transactionId $ "Proposing team owner canceled " ++ typeStr
    if transactionType transaction == Claim
        then repositionClaimRequests $ transactionTeamId transaction
        else return ()
    setMessage $ toMarkup $ typeStr ++ " canceled!"

postLeagueMoveClaimUpR :: LeagueId -> TransactionId -> Handler ()
postLeagueMoveClaimUpR _ transactionId = do
    claim <- runDB $ get404 transactionId
    let claimToMoveUp = Just $ Entity transactionId claim
    case transactionPosition claim of
        Nothing -> return ()
        Just position -> do
            claimToMoveDown <- runDB $ selectFirst [ TransactionTeamId   ==. transactionTeamId claim
                                                   , TransactionPosition ==. Just (position - 1)
                                                   ] [Asc TransactionPosition]
            swapClaimPositions claimToMoveUp claimToMoveDown
            setMessage $ toMarkup ("Claim moved up!" :: Text)

postLeagueMoveClaimDownR :: LeagueId -> TransactionId -> Handler ()
postLeagueMoveClaimDownR _ transactionId = do
    claim <- runDB $ get404 transactionId
    let claimToMoveDown = Just $ Entity transactionId claim
    case transactionPosition claim of
        Nothing -> return ()
        Just position -> do
            claimToMoveUp <- runDB $ selectFirst [ TransactionTeamId   ==. transactionTeamId claim
                                                 , TransactionPosition ==. Just (position + 1)
                                                 ] [Asc TransactionPosition]
            swapClaimPositions claimToMoveUp claimToMoveDown
            setMessage $ toMarkup ("Claim moved down!" :: Text)

swapClaimPositions :: Maybe (Entity Transaction) -> Maybe (Entity Transaction) -> Handler ()
swapClaimPositions (Just (Entity claim1Id claim1)) (Just (Entity claim2Id claim2)) = do
    case (transactionPosition claim1, transactionPosition claim2) of
        (Just claim1Position, Just claim2Position) -> do
            repositionClaimRequest claim1Id claim2Position
            repositionClaimRequest claim2Id claim1Position
        (_, _) -> return ()
swapClaimPositions _ _ = return ()

-------------
-- Widgets --
-------------
transactionRequestsPanel :: [FullTransaction] -> TransactionType -> TeamId -> Widget
transactionRequestsPanel transactions transType currentTeamId =
    let (underscore, upper, lower) = transactionRequestsAttributes transType
        groupedTransactions = groupByFirstOfSix transactions
    in  $(widgetFile "league/transaction_requests_panel")

transactionsTable :: [FullTransaction] -> Maybe TeamId -> Bool -> Widget
transactionsTable transactions maybeCurrentTeamId usePastTense =
    let groupedTransactions = groupByFirstOfSix transactions
    in  $(widgetFile "league/transactions_table")

transactionPlayerWidget :: Transaction -> FullTransactionPlayer -> TeamId -> Bool -> Widget
transactionPlayerWidget transaction fullTransactionPlayer teamId usePastTense =
    let transType = transactionType transaction
        (Entity _ transactionPlayer, _, Entity characterId character, maybeNewTeam) = fullTransactionPlayer
        newTeamIsTransTeam = Just teamId == transactionPlayerNewTeamId transactionPlayer
        (icon, pastTense, presentTense, showTeam) = transactionPlayerAttributes transType newTeamIsTransTeam
    in  $(widgetFile "league/transaction_player")

draftResults :: [FullTransaction] -> Widget
draftResults draftFullTransactions =
    let numberedTransactions = rank draftFullTransactions
        draftPicks = map (\(n, (_, t, _, p, c, _)) -> (n, t, p, c)) numberedTransactions
    in  $(widgetFile "league/draft_results_table")

tradeButton :: Text -> LeagueId -> TransactionId -> Widget
tradeButton action leagueId transactionId =
    let buttonId = action ++ "-" ++ toPathPiece leagueId ++ "-" ++ toPathPiece transactionId
        buttonClass = if action == "accept" then "btn-primary" else "btn-default" :: Text
    in  $(widgetFile "league/trade_button")

--------------------
-- Widget Helpers --
--------------------
transactionPlayerAttributes :: TransactionType -> NewTeamIsTransactionTeam ->
                               (Text, Text, Text, ShowTeamAfterCharacter)
transactionPlayerAttributes Start _     = ("level-up",   "Started",  "Start",   False)
transactionPlayerAttributes Bench _     = ("level-down", "Benched",  "Bench",   False)
transactionPlayerAttributes Draft _     = ("plus",       "Drafted",  "Draft",   False)
transactionPlayerAttributes Claim False = ("minus",      "Dropped",  "Drop",    True)
transactionPlayerAttributes Trade False = ("minus",      "Traded",   "Trade",   True)
transactionPlayerAttributes _     True  = ("plus",       "Acquired", "Acquire", False)

transactionRequestsAttributes :: TransactionType -> (Text, Text, Text)
transactionRequestsAttributes Claim = ("waiver_claims",   "Waiver Claims",   "waiver claims")
transactionRequestsAttributes Trade = ("trade_proposals", "Trade Proposals", "trade proposals")
transactionRequestsAttributes _     = error "This transaction type is not worthy of a panel"

-------------
-- Queries --
-------------
getRequestedTransactions :: LeagueId -> Maybe TeamId -> TransactionType -> Handler [FullTransaction]
getRequestedTransactions leagueId maybeTeamId transactionType = runDB
    $ E.select
    $ E.from $ \(transaction `E.InnerJoin` team `E.InnerJoin` transactionPlayer `E.InnerJoin` player `E.InnerJoin` character `E.LeftOuterJoin` newTeam) -> do
        E.on $ transactionPlayer ^. TransactionPlayerNewTeamId E.==. newTeam ?. TeamId
        E.on $ player ^. PlayerCharacterId E.==. character ^. CharacterId
        E.on $ transactionPlayer ^. TransactionPlayerPlayerId E.==. player ^. PlayerId
        E.on $ transaction ^. TransactionId E.==. transactionPlayer ^. TransactionPlayerTransactionId
        E.on $ transaction ^. TransactionTeamId E.==. team ^. TeamId
        E.where_ $
            transaction ^. TransactionType E.==. E.val transactionType E.&&.
            transaction ^. TransactionStatus E.==. E.val Requested E.&&.
            case maybeTeamId of
                Nothing -> transaction ^. TransactionLeagueId E.==. E.val leagueId
                Just teamId ->
                    (team ^. TeamId E.==. E.val teamId E.||.
                     transaction ^. TransactionOtherTeamId E.==. E.just (E.val teamId))
        E.orderBy [ E.asc (transaction ^. TransactionPosition)
                  , E.asc (transaction ^. TransactionId)
                  , E.asc (transactionPlayer ^. TransactionPlayerPlayerId)
                  ]
        return (transaction, team, transactionPlayer, player, character, newTeam)

getSuccessfulTransactions :: LeagueId -> Maybe TeamId -> Maybe TransactionType -> Handler [FullTransaction]
getSuccessfulTransactions leagueId maybeTeamId maybeTransactionType = runDB
    $ E.select
    $ E.from $ \(transaction `E.InnerJoin` team `E.InnerJoin` transactionPlayer `E.InnerJoin` player `E.InnerJoin` character `E.LeftOuterJoin` newTeam) -> do
        E.on $ transactionPlayer ^. TransactionPlayerNewTeamId E.==. newTeam ?. TeamId
        E.on $ player ^. PlayerCharacterId E.==. character ^. CharacterId
        E.on $ transactionPlayer ^. TransactionPlayerPlayerId E.==. player ^. PlayerId
        E.on $ transaction ^. TransactionId E.==. transactionPlayer ^. TransactionPlayerTransactionId
        E.on $ transaction ^. TransactionTeamId E.==. team ^. TeamId
        E.where_ $
            transaction ^. TransactionLeagueId E.==. E.val leagueId E.&&.
            transaction ^. TransactionStatus E.==. E.val Succeeded E.&&.
            case maybeTeamId of
                Just teamId -> team ^. TeamId E.==. E.val teamId E.||.
                               transaction ^. TransactionOtherTeamId E.==. E.just (E.val teamId)
                Nothing     -> case maybeTransactionType of
                                   Just transType -> transaction ^. TransactionType E.==. E.val transType
                                   Nothing        -> transaction ^. TransactionType E.!=. E.val Draft
        E.orderBy $ if maybeTransactionType == Just Draft
            then [ E.asc  (transaction ^. TransactionId) ]
            else [ E.desc (transaction ^. TransactionCompletedAt)
                 , E.desc (transaction ^. TransactionId)
                 , E.asc  (transactionPlayer ^. TransactionPlayerPlayerId)
                 ]
        return (transaction, team, transactionPlayer, player, character, newTeam)

getPlayersAndCharacters :: LeagueId -> Handler [(Entity Player, Entity Character)]
getPlayersAndCharacters leagueId = runDB
    $ E.select
    $ E.from $ \(player `E.InnerJoin` character) -> do
        E.on $ player ^. PlayerCharacterId E.==. character ^. CharacterId
        E.where_ $ player ^. PlayerLeagueId E.==. E.val leagueId
             E.&&. player ^. PlayerIsPlayable E.==. E.val True
        E.orderBy [E.asc (character ^. CharacterName)]
        return (player, character)

-------------------------
-- Create Transactions --
-------------------------
createTransaction :: Player -> Maybe (Entity Team) -> TransactionType -> Handler (Entity Transaction)
createTransaction player maybeDraftTeam transactionType = do
    userId <- requireAuthId
    now <- liftIO getCurrentTime
    let (leagueId, maybeOtherTeamId) = (playerLeagueId player, playerTeamId player)
    processableAt <- if transactionType == Claim then claimProcessableAt leagueId now else return now
    maybeTeam <- case maybeDraftTeam of
        Just draftTeam -> return $ Just draftTeam
        Nothing -> runDB $ selectFirst [TeamLeagueId ==. leagueId, TeamOwnerId ==. Just userId] []

    case maybeTeam of
        Nothing -> error "You are not a member of this league"
        Just (Entity teamId team) -> do
            position <- generateTransactionPosition teamId transactionType
            let transaction = Transaction
                    { transactionLeagueId = teamLeagueId team
                    , transactionType = transactionType
                    , transactionStatus = Requested
                    , transactionFailureReason = Nothing
                    , transactionTeamId = teamId
                    , transactionOtherTeamId = maybeOtherTeamId
                    , transactionPosition  = position
                    , transactionCreatedBy = userId
                    , transactionCreatedAt = now
                    , transactionUpdatedBy = userId
                    , transactionUpdatedAt = now
                    , transactionProcessableAt = processableAt
                    , transactionCompletedAt = Nothing
                    }
            transactionId <- runDB $ insert transaction
            return $ Entity transactionId transaction

createTransactionPlayer :: Entity Transaction -> Entity Player -> Maybe TeamId -> Handler TransactionPlayerId
createTransactionPlayer (Entity transactionId transaction) (Entity playerId player) maybeNewTeamId =
    runDB $ insert TransactionPlayer
        { transactionPlayerLeagueId      = transactionLeagueId transaction
        , transactionPlayerTransactionId = transactionId
        , transactionPlayerPlayerId      = playerId
        , transactionPlayerOldTeamId     = playerTeamId player
        , transactionPlayerNewTeamId     = maybeNewTeamId
        , transactionPlayerCreatedBy     = transactionCreatedBy transaction
        , transactionPlayerCreatedAt     = transactionCreatedAt transaction
        , transactionPlayerUpdatedBy     = transactionUpdatedBy transaction
        , transactionPlayerUpdatedAt     = transactionUpdatedAt transaction
        }

draftTransaction :: Entity Player -> Entity Team -> Handler TransactionId
draftTransaction (Entity playerId player) team = do
    (Entity transactionId transaction) <- createTransaction player (Just team) Draft
    _ <- createTransactionPlayer (Entity transactionId transaction)
         (Entity playerId player) $ playerTeamId player
    return transactionId

singlePlayerTransaction :: Entity Player -> TransactionType -> Handler TransactionId
singlePlayerTransaction (Entity playerId player) transactionType = do
    (Entity transactionId transaction) <- createTransaction player Nothing transactionType
    _ <- createTransactionPlayer (Entity transactionId transaction)
         (Entity playerId player) $ playerTeamId player
    return transactionId

twoPlayerTransaction :: Entity Player -> Entity Player -> TransactionType -> Handler TransactionId
twoPlayerTransaction (Entity player1Id player1) (Entity player2Id player2) transactionType = do
    (Entity transactionId transaction) <- createTransaction player1 Nothing transactionType
    _ <- createTransactionPlayer (Entity transactionId transaction)
         (Entity player1Id player1) $ playerTeamId player2
    _ <- createTransactionPlayer (Entity transactionId transaction)
         (Entity player2Id player2) $ playerTeamId player1
    return transactionId

generateTransactionPosition :: TeamId -> TransactionType -> Handler (Maybe Int)
generateTransactionPosition teamId Claim = do
    transactions <- runDB $ count [ TransactionTeamId ==. teamId
                                  , TransactionType   ==. Claim
                                  , TransactionStatus ==. Requested
                                  ]
    return $ Just $ transactions + 1
generateTransactionPosition _ _ = return Nothing


---------------------------
-- Process Transactions --
---------------------------
-- TODO - Actually make this go in waiver order, which it apparently doesn't
processClaimRequests :: Handler ()
processClaimRequests = do
    maybeAdmin <- runDB $ selectFirst [UserIsAdmin ==. True] [Asc UserId]
    let Entity adminUserId _ = fromJust maybeAdmin
    now <- liftIO getCurrentTime
    transactionIds <- runDB $ selectKeysList
        [ TransactionStatus ==. Requested
        , TransactionType ==. Claim
        , TransactionProcessableAt <=. now
        ] [Asc TransactionPosition, Asc TransactionId]
    mapM_ (processMultiPlayerTransaction adminUserId) transactionIds

cancelAllTransactionRequests :: UserId -> LeagueId -> Handler ()
cancelAllTransactionRequests adminUserId leagueId = do
    transactionIds <- runDB $ selectKeysList [ TransactionLeagueId ==. leagueId
                                             , TransactionStatus ==. Requested
                                             ] []
    mapM_ (cancelTransaction adminUserId) transactionIds

cancelTransaction :: UserId -> TransactionId -> Handler ()
cancelTransaction adminUserId transactionId =
    failTransactionWithUserId_ transactionId adminUserId "Season is complete"

cancelAllTrades :: UserId -> LeagueId -> Handler ()
cancelAllTrades adminUserId leagueId = do
    transactionIds <- runDB $ selectKeysList [ TransactionLeagueId ==. leagueId
                                             , TransactionStatus ==. Requested
                                             , TransactionType ==. Trade
                                             ] []
    mapM_ (cancelTrade adminUserId) transactionIds

cancelTrade :: UserId -> TransactionId -> Handler ()
cancelTrade adminUserId transactionId =
    failTransactionWithUserId_ transactionId adminUserId "Trade deadline has passed"

processMultiPlayerTransaction :: UserId -> TransactionId -> Handler ()
processMultiPlayerTransaction userId transactionId = do
    transactionPlayers <- runDB $ selectList [TransactionPlayerTransactionId ==. transactionId] []
    transactionPlayersWithPlayer <- mapM joinWithPlayer transactionPlayers
    let areTransactionPlayersValid = map isTransactionPlayerValid transactionPlayersWithPlayer 
    if foldr (&&) True areTransactionPlayersValid
        then do
            mapM_ (movePlayerToNewTeam userId) transactionPlayersWithPlayer
            succeedTransactionWithUserId transactionId userId
        else failTransactionWithUserId_ transactionId userId
                                        "One or more players not on expected team"

succeedTransaction :: TransactionId -> Handler ()
succeedTransaction transactionId = completeTransaction transactionId Nothing Nothing

succeedTransactionWithUserId :: TransactionId -> UserId -> Handler ()
succeedTransactionWithUserId transactionId userId =
    completeTransaction transactionId (Just userId) Nothing

failTransaction_ :: TransactionId -> Text -> Handler ()
failTransaction_ transactionId failureReason =
    completeTransaction transactionId Nothing $ Just failureReason

failTransactionWithUserId_ :: TransactionId -> UserId -> Text -> Handler ()
failTransactionWithUserId_ transactionId userId failureReason =
    completeTransaction transactionId (Just userId) (Just failureReason)

failTransaction :: TransactionId -> Text -> Handler Bool
failTransaction transactionId failureReason = do
    failTransaction_ transactionId failureReason
    setMessage $ toMarkup $ "Transaction failed: " ++ failureReason
    return True

completeTransaction :: TransactionId -> Maybe UserId -> Maybe Text -> Handler ()
completeTransaction transactionId maybeUserId maybeFailureReason = do
    userId <- case maybeUserId of Just uid -> return uid
                                  Nothing  -> requireAuthId
    now <- liftIO getCurrentTime
    let status = if isJust maybeFailureReason then Failed else Succeeded
    runDB $ update transactionId [ TransactionStatus =. status
                                 , TransactionFailureReason =. maybeFailureReason
                                 , TransactionUpdatedAt =. now
                                 , TransactionUpdatedBy =. userId
                                 , TransactionCompletedAt =. Just now
                                 ]

-----------------------------
--- Auto Fail Transactions --
-----------------------------
autoFailDraftTransaction :: LeagueId -> TransactionId -> Player -> Handler Bool
autoFailDraftTransaction leagueId transactionId player
    | playerTeamId player /= Nothing = 
        failTransaction transactionId "Player must not be on a team"
    | otherwise = autoFailSinglePlayerTransaction leagueId transactionId player

autoFailStartTransaction :: LeagueId -> TransactionId -> Player -> Handler Bool
autoFailStartTransaction leagueId transactionId player
    | playerIsStarter player = failTransaction transactionId "Player is already starting"
    | isNothing $ playerTeamId player = 
        failTransaction transactionId "Player must be on a team"
    | otherwise = autoFailSinglePlayerTransaction leagueId transactionId player

autoFailBenchTransaction :: LeagueId -> TransactionId -> Player -> Handler Bool
autoFailBenchTransaction leagueId transactionId player
    | not $ playerIsStarter player = failTransaction transactionId "Player is already benched"
    | isNothing $ playerTeamId player = 
        failTransaction transactionId "Player must be on a team"
    | otherwise = autoFailSinglePlayerTransaction leagueId transactionId player

autoFailClaimTransaction :: LeagueId -> TransactionId -> Player -> Player -> Handler Bool
autoFailClaimTransaction leagueId transactionId playerToAdd playerToDrop
    | playerTeamId playerToAdd /= Nothing =
        failTransaction transactionId "Desired player must not be on a team"
    | otherwise = autoFailMultiPlayerTransaction leagueId transactionId playerToAdd playerToDrop

autoFailTradeTransaction :: LeagueId -> TransactionId -> Player -> Player -> Handler Bool
autoFailTradeTransaction leagueId transactionId playerToTake playerToGive
    | playerTeamId playerToTake == Nothing =
        failTransaction transactionId "Desired player must be on a team"
    | otherwise = autoFailMultiPlayerTransaction leagueId transactionId playerToTake playerToGive

autoFailSinglePlayerTransaction :: LeagueId -> TransactionId -> Player -> Handler Bool
autoFailSinglePlayerTransaction leagueId transactionId player
    | leagueId /= playerLeagueId player =
        failTransaction transactionId "Player must be in this league"
    | not $ playerIsPlayable player = failTransaction transactionId "Player must be playable"
    | otherwise = return False

autoFailMultiPlayerTransaction :: LeagueId -> TransactionId -> Player -> Player -> Handler Bool
autoFailMultiPlayerTransaction leagueId transactionId player1 player2
    | playerLeagueId player1 /= playerLeagueId player2 =
        failTransaction transactionId "Players must be in the same league"
    | leagueId /= playerLeagueId player2 =
        failTransaction transactionId "Desired player must be in this league"
    | playerTeamId player1 == playerTeamId player2 =
        failTransaction transactionId "Players must not be on the same team"
    | not $ playerIsPlayable player1 = failTransaction transactionId "Your player must be playable"
    | not $ playerIsPlayable player2 = failTransaction transactionId "Desired player must be playable"
    | otherwise = autoFailSinglePlayerTransaction leagueId transactionId player2


---------------------
-- Generic Helpers --
---------------------
insertDraftPick :: LeagueId -> UserId -> DraftPick -> Handler ()
insertDraftPick leagueId userId draftPick = do
    let (playerId, teamId) = (draftPickPlayerId draftPick, draftPickTeamId draftPick)
    player <- runDB $ get404 playerId
    team <- runDB $ get404 teamId
    transactionId <- draftTransaction (Entity playerId player) (Entity teamId team)
    didAutoFail <- autoFailDraftTransaction leagueId transactionId player
    if didAutoFail then return () else do
        now <- liftIO getCurrentTime
        Entity _ generalSettings <- runDB $ getBy404 $ UniqueGeneralSettingsLeagueId leagueId
        let startPlayer = teamStartersCount team < generalSettingsNumberOfStarters generalSettings
        runDB $ update playerId [ PlayerIsStarter =. startPlayer
                                , PlayerTeamId =. Just teamId
                                , PlayerUpdatedBy =. userId
                                , PlayerUpdatedAt =. now
                                ]
        let teamUpdates = [TeamPlayersCount +=. 1, TeamUpdatedBy =. userId, TeamUpdatedAt =. now]
        let fullUpdates = if startPlayer then (TeamStartersCount +=. 1) : teamUpdates else teamUpdates
        runDB $ update teamId fullUpdates
        succeedTransaction transactionId

movePlayerToNewTeam :: UserId -> (Entity TransactionPlayer, Entity Player) -> Handler ()
movePlayerToNewTeam adminUserId (Entity _ transactionPlayer, Entity playerId _) = do
    now <- liftIO getCurrentTime
    runDB $ update playerId [ PlayerIsStarter =. False
                            , PlayerTeamId =. transactionPlayerNewTeamId transactionPlayer
                            , PlayerUpdatedAt =. now
                            , PlayerUpdatedBy =. adminUserId
                            ]
    case transactionPlayerNewTeamId transactionPlayer of
        Just newTeamId -> updateTeamStartersCount newTeamId now adminUserId
        Nothing -> return ()
    case transactionPlayerOldTeamId transactionPlayer of
        Just oldTeamId -> updateTeamStartersCount oldTeamId now adminUserId
        Nothing -> return ()

isTransactionPlayerValid :: (Entity TransactionPlayer, Entity Player) -> Bool
isTransactionPlayerValid (Entity _ transactionPlayer, Entity _ player) =
    playerTeamId player == transactionPlayerOldTeamId transactionPlayer

joinWithPlayer :: Entity TransactionPlayer -> Handler (Entity TransactionPlayer, Entity Player)
joinWithPlayer (Entity transactionPlayerId transactionPlayer) = do
    let playerId = transactionPlayerPlayerId transactionPlayer
    player <- runDB $ get404 playerId
    return (Entity transactionPlayerId transactionPlayer, Entity playerId player)

claimProcessableAt :: LeagueId -> UTCTime -> Handler UTCTime
claimProcessableAt leagueId utcTime = do
    Entity seriesId _ <- runDB $ getBy404 $ UniqueSeriesNumber 6
    Entity _ episode <- runDB $ getBy404 $ UniqueEpisodeNumberSeries 1 seriesId
    if utcTime < episodeAirTime episode then return utcTime else do
        Entity _ generalSettings <- runDB $ getBy404 $ UniqueGeneralSettingsLeagueId leagueId
        let daysToAdd = min 0 $ generalSettingsWaiverPeriodInDays generalSettings - dayOfWeek utcTime
        return $ if daysToAdd == 0 then utcTime else addXDays daysToAdd utcTime

updateTeamStartersCount :: TeamId -> UTCTime -> UserId -> Handler ()
updateTeamStartersCount teamId utcTime userId = runDB $ do
    startersCount <- count [PlayerTeamId ==. Just teamId, PlayerIsStarter ==. True]
    update teamId [ TeamStartersCount =. startersCount
                  , TeamUpdatedAt =. utcTime
                  , TeamUpdatedBy =. userId
                  ]

repositionClaimRequests :: TeamId -> Handler ()
repositionClaimRequests teamId = do
    claimRequests <- runDB $ selectKeysList [ TransactionTeamId ==. teamId
                                            , TransactionType   ==. Claim
                                            , TransactionStatus ==. Requested
                                            ] [Asc TransactionPosition, Asc TransactionId]
    mapM_ (\(p, t) -> repositionClaimRequest t p) $ rank claimRequests

repositionClaimRequest :: TransactionId -> Int -> Handler ()
repositionClaimRequest transactionId position = do
    userId <- requireAuthId
    now <- liftIO getCurrentTime
    runDB $ update transactionId [ TransactionPosition  =. Just position
                                 , TransactionUpdatedBy =. userId
                                 , TransactionUpdatedAt =. now
                                 ]

