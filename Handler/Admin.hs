module Handler.Admin where

import Import

import Handler.Admin.Record

import qualified Data.List            as L (init, last)
import qualified Database.Persist.Sql as S (fromSqlKey)
import           Text.Blaze                (toMarkup, Markup)
import           Text.Hamlet               (hamletFile)
import           Text.Read                 (read)
import           Text.Regex

------------
-- Fields --
------------
timeInEpisodeField :: Field Handler Int
timeInEpisodeField = Field
    { fieldParse   = parseHelper $ parseTimeInEpisode . unpack
    , fieldView    = \theId name attrs val isReq ->
        [whamlet|
          <input id=#{theId} name=#{name} *{attrs} type="text" step=any :isReq:required="" value="#{showVal val}" placeholder="mm:ss" maxlength="5"/>
        |]
    , fieldEnctype = UrlEncoded
    }
    where showVal = either id (pack . displayTime)

parseTimeInEpisode :: String -> Either FormMessage Int
parseTimeInEpisode = maybe (Left MsgInvalidTimeFormat) Right . readTimeInEpisode

readTimeInEpisode :: String -> Maybe Int
readTimeInEpisode timeInEpisode =
    let split = splitRegex (mkRegex ":") timeInEpisode
    in  case length split of 2 -> let [minutes, seconds] = map read split
                                  in  Just $ minutes * 60 + seconds
                             _ -> Nothing


-------------
-- Layouts --
-------------
adminLayout :: Text -> Text -> Widget -> Handler Html
adminLayout activeItem title widget = do
    master <- getYesod
    mmsg <- getMessage
    -- TODO - upgrade yesod-core to 1.4.20 (or higher) and use this line
    -- messages <- getMessages
    (title', parents) <- breadcrumbs
    let entityNames = ["Blurb", "Character", "Episode", "Event", "House", "Series", "Species"]
        actions  = map (activeClass activeItem) ["Score"]
        entities = map (activeClass activeItem) entityNames
        listGroups = [entities, actions]

    pc <- widgetToPageContent $ do
        addStylesheet $ StaticR css_bootstrap_css
        addStylesheet $ StaticR css_font_awesome_css
        addStylesheet $ StaticR css_bootstrap_social_css
        addStylesheet $ StaticR css_bootstrap_table_css
        addStylesheet $ StaticR css_bootstrap_override_css
        addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/2.1.0/jquery.min.js"
        addScript $ StaticR js_bootstrap_js
        addScript $ StaticR js_bootstrap_table_js
        addScript $ StaticR js_bootstrap_override_js
        setTitle $ toMarkup $ "Fantasy Game Of Thrones | Admin | " ++ title
        $(widgetFile "layouts/admin-layout")
    withUrlRenderer $(hamletFile "templates/layouts/default-layout-wrapper.hamlet")

determineActive :: Text -> Text -> Text
determineActive active toDetermine = if toDetermine == active then "active" else ""

activeClass :: Text -> Text -> (Text, Text)
activeClass activeItem item = (item, determineActive activeItem item)


-------------------
-- Common Routes --
-------------------
adminList :: (Eq record, Eq (Unique record), AdminRecord record,
              PersistUnique (PersistEntityBackend record),
              ToBackendKey SqlBackend record) =>
             String -> (Markup -> (MForm Handler (FormResult record, Widget)))
             -> Widget -> Handler Html
adminList modelLower form tableWidget = do
    (formWidget, enctype, uniqueFailureFields) <- handleFormPost modelLower form Nothing
    params <- reqGetParams <$> getRequest
    let model   = capitalize modelLower
        title   = pack $ pluralize model
        nothing = Nothing :: Maybe UserId -- need to use a Key type here...
        tabs    = [("list", "list"), ("new", "plus")] :: [(Text, Text)]
        activeTabId = if null params then "list" else "new"
    adminLayout (pack model) title $(widgetFile "admin/widgets/list")

adminShow :: (Eq record, Eq (Unique record), AdminRecord record,
               PersistUnique (PersistEntityBackend record),
               ToBackendKey SqlBackend record) =>
              String
              -> (Markup -> (MForm Handler (FormResult record, Widget)))
              -> Entity record -> Maybe (String, Widget) -> Handler Html
adminShow modelLower form (Entity entityId entity) maybeSubTable = do
    (cBy, cAt, uBy, uAt) <- auditInfo entity
    let justE = Just $ Entity entityId entity
        model = capitalize modelLower
        theId = idToString entityId
        title = pack $ model ++ " #" ++ theId
    (formWidget, enctype, uniqueFailureFields) <- handleFormPost modelLower form justE
    adminLayout (pack model) title $(widgetFile "admin/widgets/show")

-- TODO - Somehow account for all associations? Maybe it won't be as simple as this
-- Actually, I might be able to delete associations FIRST and then just call
-- this method afterwards. Maybe pass a list of associations deleted to add to
-- the message.
adminDelete :: ToBackendKey SqlBackend record => String -> Key record -> Handler ()
adminDelete model entityId = do
    runDB $ delete entityId
    let message = model ++ " #" ++ idToString entityId ++ " successfully deleted!"
    setMessage $ toMarkup message


-------------
-- Widgets --
-------------
adminTable :: String -> Widget -> Maybe String -> Widget
adminTable modelLower widget maybeHeader = $(widgetFile "admin/widgets/table")

adminActionButtons :: ToBackendKey SqlBackend record =>
                      String -> Key record -> Maybe String -> Widget
adminActionButtons modelLower entityId maybeSubModelLower = do
    let model = capitalize modelLower
        theId = idToString entityId
    $(widgetFile "admin/widgets/action_buttons")

adminForm :: ToBackendKey SqlBackend record =>
             String -> Widget -> Enctype -> [(HaskellName, DBName)] ->
             Maybe (Key record) -> Widget
adminForm modelLower widget enctype uniqueFailureFields maybeEntityId =
    let model = capitalize modelLower
        (action, title, submit) = adminFormAction modelLower maybeEntityId
    in  $(widgetFile "admin/widgets/form")

adminFormAction :: ToBackendKey SqlBackend record =>
                   String -> Maybe (Key record) -> (String, String, String)
adminFormAction modelLower Nothing = ("/admin/" ++ modelLower ++ "?new", "New", "Create")
adminFormAction modelLower (Just entityId) =
      ("/admin/" ++ modelLower ++ "/" ++ idToString entityId, "Edit", "Update")

adminDeleteModal :: String -> Widget
adminDeleteModal modelLower = let model = capitalize modelLower
                              in  $(widgetFile "admin/widgets/delete_modal")


------------------
-- Form Helpers --
------------------
handleFormPost :: (Eq record, Eq (Unique record), AdminRecord record,
                   PersistUnique (PersistEntityBackend record),
                   ToBackendKey SqlBackend record) =>
                  String
                  -> (Markup -> (MForm Handler (FormResult record, Widget)))
                  -> Maybe (Entity record)
                  -> Handler (Widget, Enctype, [(HaskellName, DBName)])
handleFormPost modelLower form maybeEntity = do
    ((result, widget), enctype) <- runFormPost form
    let maybeEntityId = map entityKey maybeEntity
        (success, failure) = handleFormPostMessages modelLower maybeEntityId
        emptyUFFList = [] :: [(HaskellName, DBName)]
    uniqueFailureFields <- case result of
        FormMissing -> return emptyUFFList
        FormFailure _ -> do
            setMessage failure
            -- TODO - upgrade yesod-core to >= 1.4.20 and use this line
            -- addMessage "danger" failure
            return emptyUFFList
        FormSuccess entity -> do
            uniqueFailureOrEntityId <- runDB $ replacertUnique maybeEntityId entity
            case uniqueFailureOrEntityId of
                Left uniqueFailure -> do
                    setMessage failure
                    -- TODO - upgrade yesod-core to >= 1.4.20 and use this line
                    -- addMessage "danger" failure
                    return $ persistUniqueToFieldNames uniqueFailure
                Right entityId -> do
                    setMessage success
                    performCallback (map entityVal maybeEntity) $ Entity entityId entity
                    redirect $ "/admin/" ++ modelLower ++ "/" ++ idToString entityId
    return (widget, enctype, uniqueFailureFields)

replacertUnique :: (Eq record, Eq (Unique record),
                   PersistUnique (PersistEntityBackend record),
                   ToBackendKey SqlBackend record) =>
                   Maybe (Key record) -> record ->
                   ReaderT SqlBackend Handler (Either (Unique record) (Key record))
replacertUnique (Just entityId) entity = do
    maybeUniqueFailure <- replaceUnique entityId entity
    return $ case maybeUniqueFailure of Just uniqueFailure -> Left uniqueFailure
                                        Nothing            -> Right entityId
replacertUnique Nothing entity = do
    maybeUniqueFailure <- checkUnique entity
    case maybeUniqueFailure of Just uniqueFailure -> return $ Left uniqueFailure
                               Nothing            -> map Right $ insert entity

handleFormPostMessages :: ToBackendKey SqlBackend record =>
                          String -> Maybe (Key record) -> (Markup, Markup)
handleFormPostMessages modelLower maybeEntityId =
    let (action, past) = if isJust maybeEntityId then ("update",   "updated")
                                                 else ("creation", "created")
        success = "Successfully " ++ past ++ " " ++ modelLower
        failure = capitalize action ++ " of " ++ modelLower ++ " failed"
    in  (toMarkup success, toMarkup failure)


-------------
-- Helpers --
-------------
pluralize :: String -> String
pluralize string = case lastSplit string of Nothing            -> []
                                            Just (_strin, 's') -> string
                                            Just ( strin, 'y') -> strin ++ "ies"
                                            Just (_strin, _)   -> string ++ "s"

seriesNumberText :: Series -> Text
seriesNumberText = pack . show . seriesNumber

alertClass :: Text -> Text
alertClass "" = "bg-primary"
alertClass status = "alert-" ++ status

lastSplit :: String -> Maybe (String, Char)
lastSplit []     = Nothing
lastSplit string = Just (L.init string, L.last string)

idToString :: ToBackendKey SqlBackend record => Key record -> String
idToString = show . S.fromSqlKey

