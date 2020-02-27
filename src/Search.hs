module Search (relatedArtistsN, Graph'(..), nodes, edges, hoistGraph) where

import Protolude hiding (getField)

import qualified Data.Set as Set
import Data.Generics.Product (getField)

import AStar (floodFillM, Graph'(..), Graph, nodes, edges, hoistGraph)
import Spotify (MonadSpotify(..), Artist, SpotifyId)

-- Build a graph of related artists to a specific depth.
relatedArtistsN :: MonadSpotify m => Artist -> Int -> m (Graph SpotifyId Artist)
relatedArtistsN a n = floodFillM a (getField @"id") neighbors n
  where
    neighbors x = do
      rs <- getRelatedArtists x
      pure $ foldl' (\acc y -> Set.insert y acc) mempty rs
