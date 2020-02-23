module AesonUtil where

import Protolude
import Prelude (String)

import qualified Data.Aeson as A
import qualified Data.Text as T

defModifier :: Text -> String -> String
defModifier p = A.camelTo2 '_' . drop (T.length p)

mkOpts :: Text -> A.Options
mkOpts p = A.defaultOptions { A.fieldLabelModifier = defModifier p }
