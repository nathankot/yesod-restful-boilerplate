module Handler.UsersSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ postUsersSpec

postUsersSpec :: SpecWith App
postUsersSpec = describe "postUsersR" $ do

  it "creates a user" $ do
    makeRequest
    statusIs 201
    Entity _ u <- runDB $ retrieve $ selectFirst [UserEmail ==. email] []
    assertEqual "correct email" "me@nathankot.com" $ userEmail u

  it "sends back an api key" $ do
    makeRequest
    statusIs 201
    valueSatisfies "Response has api key value" $ \(Object v) ->
      let Object u = v ! "user"
          Object a = u ! "apiKey"
          String s = a ! "value"
      in not (null s)

  it "sends back the user" $ do
    makeRequest
    statusIs 201
    valueSatisfies "Response has users email" $ \(Object v) ->
      let Object u = v ! "user"
          String n = u ! "email"
      in n == email

  it "fails on a malformed email address" $ do
    requestJSON $
      setUrl UsersR >>
      setMethod "POST" >>
      setRequestBody (encode $
                      object [ "email" .= ("notanemail" :: Text)
                             , "password" .= password ])
    statusIs 400

  it "fails on a short email address" $ do
    requestJSON $
      setUrl UsersR >>
      setMethod "POST" >>
      setRequestBody (encode (object
                       [ "email" .= email
                       , "password" .= ("short" :: Text) ]))
    statusIs 400

email :: Text
email = "me@nathankot.com"

password :: Text
password = "testing123" :: Text

makeRequest :: YesodExample App ()
makeRequest = requestJSON $ do
  setUrl UsersR
  setMethod "POST"
  setRequestBody $ encode $ object [ "email"     .= email
                                   , "password"  .= password ]
