{-# LANGUAGE TemplateHaskell #-}

module Spotify.Api.Types where

import Protolude

import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A
import Data.Vector (Vector)
import Control.Lens.Wrapped (Wrapped)
import Database.SQLite.Simple.ToField (ToField)
import Database.SQLite.Simple.FromField (FromField)

import AesonUtil (mkOpts)

newtype Url = Url Text
  deriving newtype (A.FromJSON, A.ToJSON, ToField, FromField)
  deriving Generic

instance Wrapped Url

newtype SpotifyId = SpotifyId Text
  deriving newtype (A.FromJSON, A.ToJSON, ToField, FromField, Eq, Ord)
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

data SpotifyObject (a :: ObjectType) = SpotifyObject
  { payload :: A.Value
  , id      :: SpotifyId
  } deriving Generic

instance Eq (SpotifyObject a) where
  SpotifyObject _ a == SpotifyObject _ b = a == b

instance Ord (SpotifyObject a) where
  compare (SpotifyObject _ a) (SpotifyObject _ b) = compare a b

instance A.FromJSON (SpotifyObject 'TArtist) where
  parseJSON v@(A.Object o) = SpotifyObject v <$> o A..: "id"
  parseJSON _ = empty

instance A.ToJSON (SpotifyObject 'TArtist) where
  toEncoding (SpotifyObject p _) = A.toEncoding p

type Artist = SpotifyObject 'TArtist

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

$(A.deriveJSON (mkOpts "") ''Followers)
$(A.deriveJSON (mkOpts "") ''Image)
$(A.deriveJSON (mkOpts "") ''ArtistObject)
$(A.deriveFromJSON (mkOpts "") ''TokenResponse)
$(A.deriveJSON (mkOpts "") ''RelatedArtistsResponse)
$(A.deriveJSON (mkOpts "") ''RelatedArtists)
