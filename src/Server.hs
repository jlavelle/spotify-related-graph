{-# LANGUAGE ViewPatterns   #-}
{-# LANGUAGE NamedFieldPuns #-}

module Server where

import Protolude hiding (getField)

import Servant.API
import Servant
import Data.Finite (Finite, getFinite)
import Data.Finite.Orphans ()
import Data.Generics.Product (getField)

import Spotify (SpotifyId, MonadSpotify)
import qualified Spotify
import qualified Search
import Server.Types (Artist, mkArtist, SearchResponse(..), RelatedGraph(..))

type GetArtist = Get '[JSON] Artist
type GetRelated = "related" :> QueryParam' '[Required] "depth" (Finite 4) :> Get '[JSON] RelatedGraph
type GetSearch = QueryParam' '[Required] "q" Text :> Get '[JSON] SearchResponse

type Api = "artist" :> "search" :> GetSearch
      :<|> "artist" :> Capture "artistid" SpotifyId :> GetArtist
      :<|> "artist" :> Capture "artistid" SpotifyId :> GetRelated

artist :: MonadSpotify m => SpotifyId -> m Artist
artist = fmap mkArtist . Spotify.getArtist

related :: MonadSpotify m => SpotifyId -> Finite 4 -> m RelatedGraph
related i (getFinite -> n) = do
  a <- Spotify.getArtist i
  RelatedGraph <$> Search.relatedArtistsN a (fromIntegral n)

search :: (MonadIO m, MonadSpotify m) => Text -> m SearchResponse
search q = toResponse <$> Spotify.search (Spotify.mkParams q [Spotify.SArtist] 20 0)
  where
    toResponse Spotify.SearchResponse{artists} = SearchResponse case artists of
      Nothing -> mempty
      Just rc -> mkArtist <$> getField @"items" rc

server :: (MonadIO m, MonadSpotify m) => ServerT Api m
server = search :<|> artist :<|> related
