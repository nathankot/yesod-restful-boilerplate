module Model.User where

import Import
import qualified Data.HashMap.Strict as H
import qualified Text.Email.Validate as EV

instance ToJSON (ResponseView (Entity User)) where
  -- Uses a whitelist approach.
  toJSON (ResponseView (Entity uid u)) =
    Object $
    H.insert "id" (toJSON uid) $
    H.delete "password" o
   where (Object o) = toJSON u

instance Validatable User where
  validations e = [ ((EV.isValid . encodeUtf8 . userEmail $ e), MsgInvalidEmailAddress) ]
