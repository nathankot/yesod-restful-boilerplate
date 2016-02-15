module Model.ApiKey where

import Import
import Network.Mail.Mime (randomString)
import System.Random
import Data.Time.Clock (addUTCTime)

instance ToJSON (ResponseView (Entity ApiKey)) where
    toJSON (ResponseView (Entity _ a)) = toJSON a

-- | Action to generate a new key for the given user.
--   The type implies that is is guaranteed to succeed,
--   because if given a persisted user, there's not reason
--   that it shouldn't.
generateApiKeyForUser :: ( MonadIO m
                         , PersistUnique backend
                         , PersistEntityBackend User ~ backend
                         , Functor m)
                      => Entity User -- ^ The already-persisted user
                      -> ReaderT backend m (Entity ApiKey)

generateApiKeyForUser u = do
    k <- liftIO generateKey
    createApiKeyForUser u k

-- | Create an API Key using the given key value.
--   However, if the value already exists, a new one will
--   be generated.
createApiKeyForUser :: ( MonadIO m
                       , PersistUnique backend
                       , PersistEntityBackend User ~ backend
                       , Functor m)
                    => Entity User -- ^ The already-persisted user
                    -> Text
                    -> ReaderT backend m (Entity ApiKey)

createApiKeyForUser u k = do
  -- Default key expiry of 1 month
  expiry <- liftIO $ addUTCTime (60 * 60 * 24 * 30) <$> getCurrentTime
  let apikey = ApiKey { apiKeyValue = k
                      , apiKeyUserId = entityKey u
                      , apiKeyExpires = expiry }
  ekid <- insertBy apikey
  case ekid of
    Left _ -> liftIO generateKey >>= createApiKeyForUser u
    Right kid -> return $ Entity kid apikey

generateKey :: IO Text
generateKey = do
  g <- newStdGen
  return $ pack $ fst $ randomString 32 g
