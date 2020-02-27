module Server.Types (RelatedGraph(..), Artist, mkArtist, SearchResponse(..)) where

import Protolude hiding (getField)

import Data.Aeson (ToJSON(..), Value, (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Encoding as A
import Data.Vector (Vector)
import Data.Profunctor (lmap)
import Data.Generics.Product (getField)
import qualified Control.Foldl as Foldl

import Control.Foldl.Aeson (arrayEncoding, arrayValue)
import qualified Spotify
import AStar (Graph'(..), Graph)

newtype Artist = Artist Value
  deriving newtype ToJSON

mkArtist :: Spotify.Artist -> Artist
mkArtist = Artist . getField @"payload"

newtype RelatedGraph = RelatedGraph (Graph Spotify.SpotifyId Spotify.Artist)

instance ToJSON RelatedGraph where
  toJSON (RelatedGraph (Graph as es)) = A.object
    [ "edges"   .= Foldl.fold (lmap (Foldl.fold (lmap toJSON arrayValue)) arrayValue) es
    , "artists" .= Foldl.fold (lmap (toJSON . mkArtist) arrayValue) as
    ]
  toEncoding (RelatedGraph (Graph as es)) =
    A.pairs (A.pair "edges" encodeEdges <> A.pair "artists" encodeArtists)
    where
      encodeEdges   = Foldl.fold (lmap (Foldl.fold (lmap toEncoding arrayEncoding)) arrayEncoding) es
      encodeArtists = Foldl.fold (lmap (toEncoding . mkArtist) arrayEncoding) as

data SearchResponse = SearchResponse
  { artists :: Vector Artist
  } deriving Generic

instance ToJSON SearchResponse
