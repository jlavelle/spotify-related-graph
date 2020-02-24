{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Cache where

import Protolude

import Data.Vector (Vector)
import qualified Data.Set as Set
import Data.FileEmbed (embedFile)
import Database.SQLite.Simple (Query(..), Connection, executeNamed, NamedParam(..), Only(..), queryNamed)
import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite3 as Sqlite3
import Linear.V2 (V2(..))
import Control.Lens ((^.))
import Data.Generics.Product (field)
import qualified Control.Foldl as Foldl
import qualified DeferredFolds.Unfoldl as Unfoldl
import qualified Data.Text as T

import Spotify.Api.Types

mkQuery :: ByteString -> Query
mkQuery = Query . toS

insertArtists :: Connection -> Vector Artist -> IO ()
insertArtists conn as = traverse_ (insertArtist conn) as

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
  *> insertGenres conn id genres
  *> insertImages conn id images
  where
    insertArtist' =
      let
        ps =
          [ ":id" := id
          , ":href" := href
          , ":follower_href" := followers ^. field @"href"
          , ":follower_count" := total followers
          , ":artist_name" := name
          , ":popularity" := popularity
          , ":uri" := uri
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
          [ ":href" := url
          , ":height" := height
          , ":width" := width
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

type ArtistRow = (SpotifyId, Url, Maybe Url, Int, Text, Int, SpotifyUri)
type ImageRow = (Url, Int, Int)

selectArtists :: Connection -> Vector SpotifyId -> IO (Vector (Either SpotifyId Artist))
selectArtists conn = traverse go
  where
    go id = maybe (Left id) Right <$> selectArtist conn id

selectArtist :: Connection -> SpotifyId -> IO (Maybe Artist)
selectArtist conn id = do
  mar <- selectArtistRow conn id
  gs  <- selectArtistGenres conn id
  is  <- selectArtistImages conn id
  pure $ mkArtist gs is <$> mar

selectArtistRow :: Connection -> SpotifyId -> IO (Maybe ArtistRow)
selectArtistRow conn id = headMay <$> queryNamed conn q ps
  where
    ps = [ ":id" := id ]
    q  = mkQuery $(embedFile "sql/select_artist_by_id.sql")

selectArtistGenres :: Connection -> SpotifyId -> IO (Vector Text)
selectArtistGenres conn id = fmap fromOnly <$> queryNamed' Foldl.vector conn q ps
  where
    ps = [ ":artist_id" := id ]
    q  = mkQuery $(embedFile "sql/select_artist_genres.sql")

selectArtistImages :: Connection -> SpotifyId -> IO (Vector Image)
selectArtistImages conn id = fmap mkImage <$> queryNamed' Foldl.vector conn q ps
  where
    ps = [ ":artist_id" := id ]
    q  = mkQuery $(embedFile "sql/select_artist_images.sql")

mkImage :: ImageRow -> Image
mkImage (url, height, width) = Image{..}

mkArtist :: Vector Text -> Vector Image -> ArtistRow -> Artist
mkArtist genres images (id, href, fhref, fcount, name, popularity, uri) =
  let
    followers = Followers fhref fcount
  in Artist{..}

selectArtistDepthSearched :: Connection -> SpotifyId -> IO Int
selectArtistDepthSearched conn id = maybe 0 identity . fmap fromOnly . headMay <$> queryNamed conn q ps
  where
    q = "select depth_searched from ArtistDepthSearched where artist_id = :artist_id"
    ps = [ ":artist_id" := id ]

upsertArtistDepthSearched :: Connection -> SpotifyId -> Int -> IO ()
upsertArtistDepthSearched conn id depth = executeNamed conn q ps
  where
    ps =
      [ ":artist_id" := id
      , ":depth_searched" := depth
      ]
    q = mkQuery $(embedFile "sql/upsert_artist_depth_searched.sql")

-- See https://github.com/nurpax/sqlite-simple/issues/44
setupDb :: Connection -> IO ()
setupDb conn = Sqlite3.exec (Sql.connectionHandle conn) q
  where
    q = toS $(embedFile "sql/setup.sql")

queryNamed' :: Sql.FromRow r => Foldl.Fold r o -> Connection -> Query -> [NamedParam] -> IO o
queryNamed' f conn q ps =
      Unfoldl.fold f
  <$> Sql.foldNamed conn q ps mempty \a r -> pure (a <> pure r)

query' :: (Sql.FromRow r, Sql.ToRow p) => Foldl.Fold r o -> Connection -> Query -> p -> IO o
query' f conn q p =
      Unfoldl.fold f
  <$> Sql.fold conn q p mempty \a r -> pure (a <> pure r)

mkParamsN :: Int -> Text
mkParamsN n = "(" <> T.intercalate "," (replicate n "?") <> ")"
