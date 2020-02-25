module Spotify (MonadSpotify(..), parseArtist, module Types) where

import Protolude

import Data.Vector (Vector)
import Data.Aeson (fromJSON, Result(..))

import Spotify.Api.Types as Types

class Monad m => MonadSpotify m where
  getArtist         :: SpotifyId -> m Artist
  getRelatedArtists :: Artist -> m (Vector Artist)

parseArtist :: Artist -> ArtistObject
parseArtist (SpotifyObject p _) = case fromJSON p of
  Success a -> a
  Error s -> panic $ toS s
