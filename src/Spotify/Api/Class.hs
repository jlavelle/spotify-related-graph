module Spotify.Api.Class where

import Protolude

import Spotify.Api.Types
import Data.Vector (Vector)

class Monad m => SpotifyPublic m where
  postCredentials   :: Credentials -> m Token
  getArtist         :: SpotifyId   -> m Artist
  getRelatedArtists :: SpotifyId   -> m (Vector Artist)
