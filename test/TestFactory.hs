module TestFactory where

import ClassyPrelude
import Model
import Database.Persist hiding (get)
import Database.Persist.Sql (SqlBackend)

factoryUser :: (MonadIO m, SqlBackend ~ backend, Functor m) =>
               (User -> User) ->
               -- ^ Chance to manipulate the user
               ReaderT backend m (Entity User)

factoryUser transform = do
  let user = transform User { userPassword = Just "notapasswordhash"
                            , userEmail    = "random@email.com" }
  uid <- insert user
  return $ Entity uid user
