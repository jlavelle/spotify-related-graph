module Database.SQLite.Simple.Extra where

import Protolude

import Database.SQLite.Simple.FromRow (FromRow)
import Database.SQLite.Simple.ToRow (ToRow)
import qualified Database.SQLite.Simple as Sql
import Database.SQLite.Simple (Connection, Query, NamedParam)
import qualified DeferredFolds.Unfoldl as Unfoldl
import DeferredFolds.Unfoldl (Unfoldl)
import Control.Foldl (Fold)

unfoldlNamed :: FromRow r => Connection -> Query -> [NamedParam] -> IO (Unfoldl r)
unfoldlNamed conn q ps = Sql.foldNamed conn q ps mempty toUnfoldl

unfoldl :: (FromRow r, ToRow ps) => Connection -> Query -> ps -> IO (Unfoldl r)
unfoldl conn q ps = Sql.fold conn q ps mempty toUnfoldl

unfoldl_ :: FromRow r => Connection -> Query -> IO (Unfoldl r)
unfoldl_ conn q= Sql.fold_ conn q mempty toUnfoldl

foldQueryNamed :: FromRow r => Fold r o -> Connection -> Query -> [NamedParam] -> IO o
foldQueryNamed f conn q ps = Unfoldl.fold f <$> unfoldlNamed conn q ps

foldQuery :: (FromRow r, ToRow ps) => Fold r o -> Connection -> Query -> ps -> IO o
foldQuery f conn q ps = Unfoldl.fold f <$> unfoldl conn q ps

foldQuery_ :: FromRow r => Fold r o -> Connection -> Query -> IO o
foldQuery_ f conn q = Unfoldl.fold f <$> unfoldl_ conn q

toUnfoldl :: Unfoldl a -> a -> IO (Unfoldl a)
toUnfoldl a r = pure (a <> pure r)
