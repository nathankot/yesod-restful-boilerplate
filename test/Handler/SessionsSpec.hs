module Handler.SessionsSpec (spec) where

import           TestImport

import qualified Data.HashMap.Strict as H
import qualified Data.Text as TS
import           Model.ApiKey
import           Network.HTTP.Types.Header
import           Yesod.Auth.Email (saltPass)

spec :: Spec
spec = withApp $ postSessionSpec >> deleteSessionSpec

postSessionSpec :: SpecWith App
postSessionSpec = describe "POST session" $ do

  it "responds with an api key" $ do
    Entity uid u <- makeUser
    makeRequest (userEmail u) pass
    statusIs 201
    Entity _ a <- runDB $ retrieve $ selectFirst [ApiKeyUserId ==. uid] []

    valueSatisfies "Api key is returned" $ \(Object v) ->
      let Object user = v ! "user"
          Object apiKey = user ! "apiKey"
      in apiKey ! "value" == (String $ apiKeyValue a) &&
         H.member "expires" apiKey

    valueSatisfies "User is returned" $ \(Object v) ->
      let Object user = v ! "user"
      in user ! "email" == (String $ userEmail u)

  where pass = "testing123456" :: Text
        makeUser = do
          salted <- liftIO $ saltPass pass
          user <- runDB $ factoryUser $ \u ->
            u { userPassword = Just salted }
          return user

        makeRequest email password = requestJSON $ do
          setMethod "POST"
          setUrl SessionsR
          setRequestBody $ encode $
            object [ "email" .= email
                   , "password" .= password ]

deleteSessionSpec :: SpecWith App
deleteSessionSpec = describe "DELETE session" $ do

  it "removes the api key" $ do
    user@(Entity uid _) <- runDB $ factoryUser id
    Entity _ k <- runDB $ generateApiKeyForUser user

    requestJSON $ do
      setUrl SessionsR
      setMethod "DELETE"
      addRequestHeader (hAuthorization, encodeUtf8 $ "Bearer " `TS.append` apiKeyValue k)

    statusIs 204
    ma <- runDB $ selectFirst [ApiKeyUserId ==. uid] []
    assertEqual "API key no longer exists" False $ isJust ma
