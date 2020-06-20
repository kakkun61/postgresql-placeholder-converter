module Database.PostgreSQL.Placeholder.Convert
  ( convertQuestionMarkStyleToDollarSignStyle
  , QD.splitQueries
  ) where

import qualified Database.PostgreSQL.Placeholder.Convert.QuestionToDollar as QD

import Data.ByteString (ByteString)

-- | Convert question mark style to dollar sign style of PostgreSQL SQL.
convertQuestionMarkStyleToDollarSignStyle :: ByteString-> Either String ByteString
convertQuestionMarkStyleToDollarSignStyle = QD.convert
