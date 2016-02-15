module Handler.Users where

import Import
import Data.Aeson
import Model.User ()
import Model.ApiKey
import Handler.Sessions (SessionResponse (..))
import Yesod.Auth.Email (saltPass)

data UserRequest = UserRequest
                         { userRequestEmail     :: Text
                         , userRequestPassword  :: Text }

instance Validatable UserRequest where
  validations e = [ ((6<=) . length . userRequestPassword $ e, (MsgPasswordTooShort 6)) ]

instance FromJSON UserRequest where
  parseJSON = withObject "user" $ \o ->
    UserRequest
    <$> o .: "email"
    <*> o .: "password"

postUsersR :: Handler Value
postUsersR = do
  userreq <- requireJsonBody :: Handler UserRequest
  let (isValid, errors) = validate userreq
  unless isValid $ invalidArgsI errors
  salted <- liftIO . saltPass . userRequestPassword $ userreq
  let u = User (userRequestEmail userreq) (Just salted)
  let (isValid', errors') = validate u
  unless isValid' $ invalidArgsI errors'
  uid <- fromMaybeM (invalidArgsI [MsgEmailTaken]) $ runDB (insertUnique u)
  let user = Entity uid u
  apikey <- runDB $ generateApiKeyForUser user
  sendResponseStatus status201 $ object ["user" .= SessionResponse user apikey]
