module Spotify (MonadSpotify(..), parseArtist, module Types, mkParams) where

import Protolude

import Data.Vector (Vector)
import Data.Aeson (fromJSON, Result(..))
import qualified Data.List.NonEmpty as NE

import Spotify.Api.Types as Types

class Monad m => MonadSpotify m where
  getArtist         :: SpotifyId -> m Artist
  getRelatedArtists :: SpotifyId -> m (Vector Artist)
  search            :: SearchParams -> m SearchResponse

mkParams :: Text -> [SearchType] -> Int -> Int -> SearchParams
mkParams t xs l o = SearchParams t st l o
  where
    st | null xs   = NE.fromList [SArtist]
       | otherwise = NE.fromList xs

parseArtist :: Artist -> ArtistObject
parseArtist (SpotifyObject p _) = case fromJSON p of
  Success a -> a
  Error s -> panic $ toS s
