-- | Common handler functions.
module Handler.Common where

import Import

import Data.FileEmbed (embedFile)

-- These handlers embed files in the executable at compile time to avoid a
-- runtime dependency, and for efficiency.

getFaviconR :: Handler TypedContent
getFaviconR = return $ TypedContent "image/x-icon"
                     $ toContent $(embedFile "config/favicon.ico")

getRobotsR :: Handler TypedContent
getRobotsR = return $ TypedContent typePlain
                    $ toContent $(embedFile "config/robots.txt")

getLetsEncryptR :: Text -> Handler Text
getLetsEncryptR text = do
    master <- getYesod
    if text == appAcmeChallenge master
        then return $ appLetsEncrypt master
        else redirect $ HomeR
