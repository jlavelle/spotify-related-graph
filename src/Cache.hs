{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Cache where

import Protolude

import Data.Vector (Vector)
import qualified Data.Set as Set
import Data.FileEmbed (embedFile)
import Database.SQLite.Simple (Query(..), withTransaction, Connection, executeNamed, NamedParam(..), Only(..), execute_)
import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite3 as Sqlite3
import Linear.V2 (V2(..))

import Spotify.Api.Types

mkQuery :: ByteString -> Query
mkQuery = Query . toS

insertArtistRelation :: Connection -> SpotifyId -> SpotifyId -> IO ()
insertArtistRelation conn id1 id2 = executeNamed conn q ps
  where
    ps =
      [ ":artist1_id" := id1
      , ":artist2_id" := id2
      ]
    q = mkQuery $(embedFile "sql/insert_artist_relation.sql")

insertArtist :: Connection -> Artist -> IO ()
insertArtist conn Artist{..} =
     insertArtist'
  *> insertGenres conn _artistId _artistGenres
  *> insertImages conn _artistId _artistImages
  where
    insertArtist' =
      let
        ps =
          [ ":id" := _artistId
          , ":href" := _artistHref
          , ":follower_href" := _followersHref _artistFollowers
          , ":follower_count" := _followersTotal _artistFollowers
          , ":artist_name" := _artistName
          , ":popularity" := _artistPopularity
          , ":uri" := _artistUri
          ]
      in executeNamed conn q ps
    q = mkQuery $(embedFile "sql/insert_artist.sql")

insertGenres :: Connection -> SpotifyId -> Vector Text -> IO ()
insertGenres conn id gs = traverse_ go gs
  where
    go t =
         executeNamed conn q1 [":genre_name" := t]
      *> executeNamed conn q2 [":genre_name" := t, ":artist_id" := id]

    q1 = mkQuery $(embedFile "sql/insert_genre.sql")
    q2 = mkQuery $(embedFile "sql/insert_artist_genre.sql")

insertImages :: Connection -> SpotifyId -> Vector Image -> IO ()
insertImages conn id gs = traverse_ go gs
  where
    go Image{..} =
      let
        ps =
          [ ":href" := _imageUrl
          , ":height" := _imageHeight
          , ":width" := _imageWidth
          , ":artist_id" := id
          ]
      in executeNamed conn q ps
    q = mkQuery $(embedFile "sql/insert_artist_image.sql")

selectArtistIds :: Connection -> IO (Set SpotifyId)
selectArtistIds conn = Sql.fold_ conn "select id from Artist" Set.empty go
  where
    go :: Set SpotifyId -> Only SpotifyId -> IO (Set SpotifyId)
    go acc (Only x) = pure $ Set.insert x acc

selectArtistRelations :: Connection -> IO (Set (V2 SpotifyId))
selectArtistRelations conn =
  Sql.fold_
    conn
    "select artist1_id, artist2_id from ArtistRelation"
    Set.empty
    go
  where
    go :: Set (V2 SpotifyId) -> (SpotifyId, SpotifyId) -> IO (Set (V2 SpotifyId))
    go acc (id1, id2) = pure $ Set.insert (V2 id1 id2) acc

-- See https://github.com/nurpax/sqlite-simple/issues/44
setupDb :: Connection -> IO ()
setupDb conn = Sqlite3.exec (Sql.connectionHandle conn) q
  where
    q = toS $(embedFile "sql/setup.sql")
