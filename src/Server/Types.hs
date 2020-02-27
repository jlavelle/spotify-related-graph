module Server.Types (RelatedGraph(..), Artist, mkArtist, SearchResponse(..)) where

import Protolude hiding (getField)

import Data.Aeson (ToJSON(..), Value, (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Encoding as A
import Data.Vector (Vector)
import Data.Generics.Product (getField)

import qualified Spotify
import AStar (Graph'(..))

newtype Artist = Artist Value
  deriving newtype ToJSON

mkArtist :: Spotify.Artist -> Artist
mkArtist = Artist . getField @"payload"

newtype RelatedGraph = RelatedGraph (Graph' Vector Spotify.SpotifyId Artist)

instance ToJSON RelatedGraph where
  toJSON (RelatedGraph (Graph as es)) = A.object
   [ "edges" .= fmap toList es
   , "artists" .= as
   ]
  toEncoding (RelatedGraph (Graph as es)) =
    A.pairs (A.pair "edges" (encodeEdges es) <> A.pair "artists" (toEncoding as))
    where
      encodeEdges = toEncoding . fmap toList

data SearchResponse = SearchResponse
  { artists :: Vector Artist
  } deriving Generic

instance ToJSON SearchResponse
