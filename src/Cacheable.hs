{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}

module Cacheable where

import Protolude hiding (HasField, getField, try)

import qualified Database.SQLite.Simple as Sql
import Database.SQLite.Simple.ToField (ToField(..))
import Database.SQLite.Simple.FromField (FromField)
import Database.SQLite.Simple (Connection, executeNamed, execute_, queryNamed, Query(..), NamedParam(..), Only(..), SQLData(..))
import Data.Generics.Product (HasField', getField)
import Data.Aeson (ToJSON, FromJSON, encode, decode)
import NeatInterpolation (text)
import qualified Control.Foldl as Foldl
import Data.Vector (Vector)
import Control.Monad.Catch (MonadCatch, MonadThrow, try, throwM)
import Data.Pool (Pool, withResource)
import Control.Monad.Trans.Control (MonadBaseControl)

import Database.SQLite.Simple.Extra (foldQuery_)

-- TODO Invalidate function
class Cacheable m i a | a -> i where
  cache  :: a -> m ()
  lookup :: i -> m (Maybe a)
  allIds :: m (Vector i)

type SqliteCtx r m =
  ( HasField' "pool" r (Pool Connection)
  , MonadReader r m
  , MonadCatch m
  , MonadThrow m
  , MonadIO m
  , MonadBaseControl IO m
  )

withCache :: (Monad m, Cacheable m i a) => (i -> m a) -> i -> m a
withCache f i = do
  mr <- lookup i
  case mr of
    Just r  -> pure r
    Nothing -> f i >>= \r -> cache r $> r

withCache' :: (Monad m, Cacheable m i a) => (b -> i) -> (b -> m a) -> b -> m a
withCache' p f b = do
  mr <- lookup $ p b
  case mr of
    Just r  -> pure r
    Nothing -> f b >>= \r -> cache r $> r

mkCacheFn :: (SqliteCtx r m, ToJSON a, ToField i) => Text -> (a -> i) -> a -> m ()
mkCacheFn tableName getId a = withCreateTable (toField $ getId a) tableName do
  let ps = [":id" := getId a, ":data" := encode a ]
  pool <- ask <&> getField @"pool"
  withResource pool \conn -> liftIO $ executeNamed conn q ps
  where
    q = Query
      [text|
        insert into $tableName (id, data)
        values (:id, :data)
        on conflict(id) do update set data = excluded.data
      |]

mkLookupFn :: (SqliteCtx r m, FromJSON a, ToField i) => Text -> i -> m (Maybe a)
mkLookupFn tableName i = withCreateTable (toField i) tableName do
  let ps = [ ":id" := i ]
  pool <- ask <&> getField @"pool"
  rs <- withResource pool \conn -> liftIO $ queryNamed conn q ps
  pure $ headMay rs >>= decode . fromOnly
  where
    q = Query [text|select data from $tableName where id = :id|]

mkCreateTable :: SqliteCtx r m => SQLData -> Text -> m ()
mkCreateTable it tableName = do
  pool <- ask <&> getField @"pool"
  withResource pool \conn -> liftIO $ execute_ conn q
  where
    idType = nameOf it
    q = Query
      [text|
        create table if not exists $tableName(
          id $idType primary key,
          data blob not null
        )
      |]

withCreateTable :: SqliteCtx r m => SQLData -> Text -> m a -> m a
withCreateTable t tableName act = do
  er <- try act
  case er of
    Right a -> pure a
    Left (Sql.SQLError Sql.ErrorError _ _) -> do
      mkCreateTable t tableName
      act
    Left e -> throwM e

mkAllIds :: (SqliteCtx r m, FromField i) => Text -> m (Vector i)
mkAllIds tableName = do
  pool <- ask <&> getField @"pool"
  rs <- withResource pool \conn -> liftIO $ foldQuery_ Foldl.vector conn q
  pure $ fromOnly <$> rs
  where
    q = Query [text|select id from $tableName|]

nameOf :: SQLData -> Text
nameOf = \case
  SQLInteger _ -> "integer"
  SQLFloat   _ -> "real"
  SQLText    _ -> "text"
  SQLBlob    _ -> "blob"
  SQLNull      -> panic "Null is not allowed as a column type."
