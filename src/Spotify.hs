module Spotify (MonadSpotify(..), module Types) where

import Protolude
import Data.Vector (Vector)

import Spotify.Api.Types as Types

class Monad m => MonadSpotify m where
  getArtist         :: SpotifyId -> m Artist
  getRelatedArtists :: Artist -> m (Vector Artist)
