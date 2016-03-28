module Model.Types where

import ClassyPrelude.Yesod
import Web.PathPieces

-----------------
-- ScoringType --
-----------------
data ScoringType = Vanilla | Weighted | Scorekeeper
    deriving (Show, Read, Eq, Ord, Enum, Bounded)
derivePersistField "ScoringType"

instance PathPiece ScoringType where
    fromPathPiece = readFromPathPiece
    toPathPiece = showToPathPiece

scoringTypeDescription :: ScoringType -> String
scoringTypeDescription Vanilla =
    "Standard scoring. All actions that we keep track of are worth the same amount of points for each player."
scoringTypeDescription Weighted =
    "Actions earns points based on their importance, so Tyrion killing Tywin would score more than Stannis killing a random Lannister."
scoringTypeDescription Scorekeeper =
    "Don't want to use our scoring system? Choose your own scorekeeper to determine which characters deserve the most points!"

scoringTypes :: [ScoringType]
scoringTypes = [minBound .. maxBound] :: [ScoringType]

isRecommendedScoringType :: ScoringType -> Bool
isRecommendedScoringType Weighted = True
isRecommendedScoringType _        = False


---------------
-- DraftType --
---------------
data DraftType = Offline | Live | Autopick
    deriving (Show, Read, Eq, Ord, Enum, Bounded)
derivePersistField "DraftType"

instance PathPiece DraftType where
    fromPathPiece = readFromPathPiece
    toPathPiece = showToPathPiece

draftTypeDescription :: DraftType -> String
draftTypeDescription Offline =
    "Your league does the draft offline, and you submit the results manually."
draftTypeDescription Live =
    "Your league does a live draft on our site"
draftTypeDescription Autopick =
    "Your league's rosters are drafted based on each team's pre-draft ranking list, and results are sent by email."

draftTypes :: [DraftType]
draftTypes = [minBound .. maxBound] :: [DraftType]

isRecommendedDraftType :: DraftType -> Bool
isRecommendedDraftType Offline = True
isRecommendedDraftType _       = False


-----------------
-- Draft Order --
-----------------
data DraftOrder = Snake | Linear
    deriving (Show, Read, Eq, Ord, Enum, Bounded)
derivePersistField "DraftOrder"

instance PathPiece DraftOrder where
    fromPathPiece = readFromPathPiece
    toPathPiece = showToPathPiece

draftOrders :: [DraftOrder]
draftOrders = [minBound .. maxBound] :: [DraftOrder]

draftOrderOptions :: [(Text, DraftOrder)]
draftOrderOptions = map (\draftOrder ->
    (pack $ show draftOrder, draftOrder)) draftOrders 

--------------------
-- DraftOrderType --
--------------------
data DraftOrderType = ManuallySet | RandomNow | RandomLater
    deriving (Show, Read, Eq, Ord, Enum, Bounded)
derivePersistField "DraftOrderType"

instance PathPiece DraftOrderType where
    fromPathPiece = readFromPathPiece
    toPathPiece = showToPathPiece

draftOrderTypeDescription :: DraftOrderType -> String
draftOrderTypeDescription ManuallySet = "Manually set by league manager"
draftOrderTypeDescription RandomNow   = "Randomized at league creation"
draftOrderTypeDescription RandomLater = "Randomized at draft time"

draftOrderTypes :: [DraftOrderType]
draftOrderTypes = [minBound .. maxBound] :: [DraftOrderType]

draftOrderTypeOptions :: [(Text, DraftOrderType)]
draftOrderTypeOptions = map (\draftOrderType ->
    (pack $ draftOrderTypeDescription draftOrderType, draftOrderType)) draftOrderTypes 


----------------------
-- PlayersTableType --
----------------------
data PlayersTableType = FreeAgents | OnRosters | AllPlayers | SingleTeam Text | Players Int Int | PlayersModal Int Int
    deriving (Show, Read, Eq, Ord)

isMultipleTeams :: PlayersTableType -> Bool
isMultipleTeams OnRosters  = True
isMultipleTeams AllPlayers = True
isMultipleTeams _ = False

hasSlotColumn :: PlayersTableType -> Bool
hasSlotColumn (Players _ _) = True
hasSlotColumn (PlayersModal _ _) = True
hasSlotColumn _ = False

hasRadioButton :: PlayersTableType -> Bool
hasRadioButton (PlayersModal _ _) = True
hasRadioButton _ = False

showActionButton :: PlayersTableType -> Bool
showActionButton (PlayersModal _ _) = False
showActionButton _ = True

splitPlayersTable :: PlayersTableType -> Maybe (Int, Int)
splitPlayersTable (Players numberOfStarters rosterSize) = Just (numberOfStarters, rosterSize)
splitPlayersTable (PlayersModal numberOfStarters rosterSize) = Just (numberOfStarters, rosterSize)
splitPlayersTable _ = Nothing

playersTableColumnCount :: PlayersTableType -> Int
playersTableColumnCount ptt
    | hasRadioButton ptt = 4
    | isMultipleTeams ptt || hasSlotColumn ptt = 3
    | otherwise = 2

packPlayersTableType :: PlayersTableType -> Text
packPlayersTableType FreeAgents = "Free Agents"
packPlayersTableType OnRosters  = "On Rosters"
packPlayersTableType AllPlayers = "All Players"
packPlayersTableType (SingleTeam teamName) = "House " ++ teamName ++ " players"
packPlayersTableType (Players _ _) = "Players"
packPlayersTableType (PlayersModal _ _) = "Players"


---------------------
-- TransactionType --
---------------------
data TransactionType = Start | Bench | Draft | Claim | Trade
    deriving (Show, Read, Eq, Ord, Enum, Bounded)
derivePersistField "TransactionType"

instance PathPiece TransactionType where
    fromPathPiece = readFromPathPiece
    toPathPiece = showToPathPiece


-----------------------
-- TransactionStatus --
-----------------------
data TransactionStatus = Requested | Succeeded | Failed
    deriving (Show, Read, Eq, Ord, Enum, Bounded)
derivePersistField "TransactionStatus"

instance PathPiece TransactionStatus where
    fromPathPiece = readFromPathPiece
    toPathPiece = showToPathPiece

