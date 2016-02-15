module Handler.Sessions where

import           Import
import           Model.User ()
import           Model.ApiKey
import           Control.Monad.Trans.Maybe
import qualified Data.HashMap.Strict as H
import           Data.Aeson (withObject)
import           Yesod.Auth.Email (isValidPass)

type Email = Text
type Password = Text
data SessionRequest = SessionRequest Email Password

instance FromJSON SessionRequest where
  parseJSON = withObject "session request" $ \o ->
    SessionRequest
    <$> o .: "email"
    <*> o .: "password"

data SessionResponse = SessionResponse (Entity User) (Entity ApiKey)

instance ToJSON SessionResponse where
  -- | Merges the given api key into the given user.
  toJSON (SessionResponse u apikey) = Object
    $ H.insert "apiKey" (toJSON (ResponseView apikey))
    $ o
    where Object o = toJSON (ResponseView u)

-- | Re-implementation of 'auth-email's login handler,
--   heavily based on: https://goo.gl/yEQyqt
postSessionsR :: Handler Value
postSessionsR = do
  SessionRequest email pass <- requireJsonBody :: Handler SessionRequest

  mu <- runMaybeT $ do
    Entity uid u <- MaybeT $ runDB $ getBy $ UniqueUser email
    realpass <- MaybeT $ return $ userPassword u
    if isValidPass pass realpass
      then (MaybeT . runDB . get) uid >>= MaybeT . return . Just . Entity uid
      else MaybeT $ return Nothing

  case mu of
    -- If we have a user, that mean this user has authenticated successfully
    Just user -> do
      a <- runDB $ generateApiKeyForUser user
      sendResponseStatus status201 $ object ["user" .= SessionResponse user a]

    -- Otherwise, assume that authentication didn't go so well
    _ -> do
      mr <- getMessageRender
      sendResponseStatus status401 $ object
        ["message" .= mr MsgInvalidCredentials]

-- | Delete a session and remove it's API key.
--   This is effectively logging out.
deleteSessionsR :: Handler Value
deleteSessionsR = do
  _ <- runMaybeT $ do
    authorization <- MaybeT $ lookupHeader hAuthorization
    k <- MaybeT $ return $ stripPrefix "Bearer " authorization
    Entity aid _ <- MaybeT . runDB . getBy $ UniqueApiKey (decodeUtf8 k)
    MaybeT $ Just <$> runDB (delete aid)

  sendResponseStatus status204 Null
