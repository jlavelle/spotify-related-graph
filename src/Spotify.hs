module Spotify (MonadSpotify(..), module X) where

import Protolude
import Data.Vector (Vector)

import qualified Spotify.Api.Types as X
import Spotify.Api.Types (Artist, Credentials, Token, SpotifyId)

class Monad m => MonadSpotify m where
  getAuthToken      :: Credentials -> m Token
  getArtist         :: Token -> SpotifyId -> m Artist
  getRelatedArtists :: Token -> SpotifyId -> m (Vector Artist)
