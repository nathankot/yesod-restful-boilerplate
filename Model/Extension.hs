module Model.Extension where

import ClassyPrelude.Yesod
import Foundation
import Control.Monad.Writer.Strict hiding (foldM)

newtype RequestView e = RequestView e
newtype ResponseView e = ResponseView e

-- | Represents an entity that can accept user-input
--   updates safely. Basically this means that the @Entity@
--   defines a specific set of whitelisted updatable properties.
class Updatable e where
    updatableProps :: e -> [Text]
    updatableProps _ = []

-- | Represents an entity that has validation logic
class Validatable e where
    -- | A set of validations and error messages for a
    --   given entity.
    validations :: e -> [(Bool, AppMessage)]
    validations _ = []

    -- | Validate an entity and respond with a Bool wrapped in
    --   a writer with potential error messages. By default this
    --   makes use of @validations e@
    validate :: e -> (Bool, [AppMessage])
    validate e = runWriter $ foldM folder True $ validations e
      where
        folder a (v, m) | v = return $ a && True
                        | otherwise = tell [m] >> return False
