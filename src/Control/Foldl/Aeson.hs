module Control.Foldl.Aeson where

import Protolude

import Control.Foldl (Fold(..))
import qualified Control.Foldl as Foldl
import Data.Aeson (Value(..))
import Data.Aeson.Encoding (Encoding, Encoding', emptyArray_, emptyObject_, pairs)
import qualified Data.Aeson.Encoding.Internal as Encoding
import Data.Aeson.Encoding.Internal (closeBracket, openBracket, comma, (><), retagEncoding, colon)
import qualified Data.HashMap.Strict as HashMap

-- Fold into a JSON Array Encoding
arrayEncoding :: Fold Encoding Encoding
arrayEncoding = Fold step Nothing (maybe emptyArray_ (>< closeBracket))
  where
    step Nothing a  = Just $ openBracket >< a
    step (Just e) a = Just $ e >< comma >< a

-- Fold into a JSON Array Value
arrayValue :: Fold Value Value
arrayValue = Array <$> Foldl.vector

dictEncoding :: Fold (Encoding' Text, Encoding) Encoding
dictEncoding = Fold step Nothing (maybe emptyObject_ pairs)
  where
    step Nothing kv  = Just $ encodeKv kv
    step (Just x) kv = Just $ encodeKv kv <> x
    encodeKv (k, v) = Encoding.Value $ retagEncoding k >< colon >< retagEncoding v

dictValue :: Fold (Text, Value) Value
dictValue = Fold step HashMap.empty Object
  where
    step m (k, v) = HashMap.insert k v m
