module Search where

import Protolude

import Linear.V2 (V2(..))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Lens (view, (^.), ifoldl')

import AStar (astarM, gcost, scores, edges)
import Spotify (MonadSpotify(..), Artist)

-- Build an edge set of related artists to a specific depth.  The set will be empty
-- if the search was unable to reach the requested depth.
relatedArtistsN :: MonadSpotify m => Artist -> Int -> m (Set (V2 Artist))
relatedArtistsN a n = toEdgeSet <$> astarM a (0 :: Int) done (\_ _ -> pure 0) neighbors
  where
    toEdgeSet = \case
      Nothing        -> mempty
      Just (g, _, s) -> ifoldl' go mempty $ s ^. edges & Map.filter (/= g)
        where
          go i acc x = Set.insert (V2 i x) acc
    done _ s = pure $ maximumBy (comparing $ view gcost) (s ^. scores) ^. gcost >= n
    neighbors x _ = do
      rs <- getRelatedArtists x
      pure $ foldl' (\acc y -> Map.insert y 1 acc) mempty rs
