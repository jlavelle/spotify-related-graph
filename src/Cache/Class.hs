module Cache.Class where

import Protolude

import Data.Vector (Vector)
import Linear.V2 (V2)
import Spotify.Api.Types

class Monad m => MonadCache m where
  cacheArtists        :: Vector Artist -> m ()
  lookupArtists       :: Vector SpotifyId -> m (Vector (Either SpotifyId Artist))
  cachedIds           :: m (Set SpotifyId)
  cachedRelations     :: m (Set (V2 SpotifyId))
  lookupDepthSearched :: SpotifyId -> m Int
  cacheDepthSearched  :: SpotifyId -> Int -> m ()
