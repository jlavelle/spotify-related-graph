{-# LANGUAGE TemplateHaskell #-}

module Spotify.Api.Types where

import Protolude

import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A
import Data.Vector (Vector)
import Control.Lens.Wrapped (Wrapped)
import Database.SQLite.Simple.ToField (ToField)
import Database.SQLite.Simple.FromField (FromField)
import Data.Hashable (Hashable)
import Servant.API (FromHttpApiData)

import AesonUtil (mkOpts)

newtype Url = Url Text
  deriving newtype (A.FromJSON, A.ToJSON, ToField, FromField)
  deriving Generic

instance Wrapped Url

newtype SpotifyId = SpotifyId Text
  deriving newtype (A.FromJSON, A.ToJSON, ToField, FromField, Eq, Ord, FromHttpApiData)
  deriving Generic

instance Wrapped SpotifyId

newtype SpotifyUri = SpotifyUri Text
  deriving newtype (A.FromJSON, A.ToJSON, ToField, FromField)
  deriving Generic

instance Wrapped SpotifyUri

newtype Credentials = Credentials ByteString
  deriving Generic

instance Wrapped Credentials

newtype Token = Token Text
  deriving newtype A.FromJSON
  deriving Generic

instance Wrapped Token

data ObjectType
  = TArtist
  | TAlbumSimple
  | TTrack
  | TPlaylist

data SpotifyObject (a :: ObjectType) = SpotifyObject
  { payload :: A.Value
  , id      :: SpotifyId
  } deriving Generic

instance Eq (SpotifyObject a) where
  SpotifyObject _ a == SpotifyObject _ b = a == b

instance Ord (SpotifyObject a) where
  compare (SpotifyObject _ a) (SpotifyObject _ b) = compare a b

instance A.FromJSON (SpotifyObject a) where
  parseJSON v@(A.Object o) = SpotifyObject v <$> o A..: "id"
  parseJSON _ = empty

instance A.ToJSON (SpotifyObject a) where
  toEncoding (SpotifyObject p _) = A.toEncoding p

type Artist = SpotifyObject 'TArtist
type AlbumSimple = SpotifyObject 'TAlbumSimple
type Track = SpotifyObject 'TTrack
type Playlist = SpotifyObject 'TPlaylist

data ArtistObject = ArtistObject
  { followers  :: Followers
  , genres     :: Vector Text
  , href       :: Url
  , id         :: SpotifyId
  , images     :: Vector Image
  , name       :: Text
  , popularity :: Int
  , uri        :: SpotifyUri
  } deriving Generic

data Followers = Followers
  { href  :: Maybe Url
  , total :: Int
  } deriving Generic

data Image = Image
  { height :: Int
  , width  :: Int
  , url    :: Url
  } deriving Generic

data TokenResponse = TokenResponse
  { accessToken :: Token
  , tokenType   :: Text
  , expiresIn   :: Int
  } deriving Generic

data RelatedArtistsResponse = RelatedArtistsResponse
  { artists :: Vector Artist
  } deriving Generic

data RelatedArtists = RelatedArtists
  { parent   :: SpotifyId
  , children :: Vector Artist
  } deriving Generic

data SearchType
  = SAlbum
  | SArtist
  | STrack
  | SPlaylist
  deriving Generic

instance Hashable SearchType

data SearchParams = SearchParams
  { query      :: Text
  , searchType :: NonEmpty SearchType
  , limit      :: Int
  , offset     :: Int
  } deriving Generic

instance Hashable SearchParams

data ResultContainer a = ResultContainer
  { href     :: Url
  , items    :: Vector a
  , limit    :: Int
  , next     :: Maybe Url
  , offset   :: Int
  , previous :: Maybe Url
  , total    :: Int
  } deriving Generic

data SearchResponse = SearchResponse
  { artists   :: Maybe (ResultContainer Artist)
  , albums    :: Maybe (ResultContainer AlbumSimple)
  , tracks    :: Maybe (ResultContainer Track)
  , playlists :: Maybe (ResultContainer Playlist)
  } deriving Generic

$(A.deriveJSON (mkOpts "") ''Followers)
$(A.deriveJSON (mkOpts "") ''Image)
$(A.deriveJSON (mkOpts "") ''ArtistObject)
$(A.deriveFromJSON (mkOpts "") ''TokenResponse)
$(A.deriveJSON (mkOpts "") ''RelatedArtistsResponse)
$(A.deriveJSON (mkOpts "") ''RelatedArtists)
$(A.deriveJSON (mkOpts "") ''ResultContainer)
$(A.deriveJSON (mkOpts "") ''SearchResponse)
