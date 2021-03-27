{-# LANGUAGE CPP               #-}

module Database.PostgreSQL.Placeholder.Convert
  ( convertQuestionMarkStyleToDollarSignStyle
  , QD.splitQueries
  ) where

import qualified Database.PostgreSQL.Placeholder.Convert.QuestionToDollar as QD

import Data.ByteString (ByteString)

#if !MIN_VERSION_base(4,13,0)
import           Control.Monad.Fail               (MonadFail)
#endif

-- | Convert question mark style to dollar sign style of PostgreSQL SQL.
--
-- >>> :set -XOverloadedStrings
-- >>> convertQuestionMarkStyleToDollarSignStyle "SELECT * FROM person WHERE id = ?"
-- "SELECT * FROM person WHERE id = $1"
convertQuestionMarkStyleToDollarSignStyle :: MonadFail m => ByteString -> m ByteString
convertQuestionMarkStyleToDollarSignStyle = QD.convert
