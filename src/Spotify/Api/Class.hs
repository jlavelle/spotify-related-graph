module Spotify.Api.Class where

import Protolude

import Spotify.Api.Types
import Data.Vector (Vector)

class Monad m => MonadSpotify m where
  getAuthToken      :: Credentials -> m Token
  getArtist         :: Token -> SpotifyId -> m Artist
  getRelatedArtists :: Token -> SpotifyId -> m (Vector Artist)
