{-# LANGUAGE TemplateHaskell #-}

module Spotify.Api.Types where

import Protolude

import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A
import Data.Vector (Vector)
import Control.Lens ((^.))
import Control.Lens.Wrapped (Wrapped)
import Database.SQLite.Simple.ToField (ToField)
import Database.SQLite.Simple.FromField (FromField)
import Data.Generics.Product (field)

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

data Artist = Artist
  { followers  :: Followers
  , genres     :: Vector Text
  , href       :: Url
  , id         :: SpotifyId
  , images     :: Vector Image
  , name       :: Text
  , popularity :: Int
  , uri        :: SpotifyUri
  } deriving Generic

instance Eq Artist where
  a == b = a ^. field @"id" == b ^. field @"id"

instance Ord Artist where
  compare a b = compare (a ^. field @"id") (b ^. field @"id")

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

data RelatedArtists = RelatedArtists
  { parent   :: SpotifyId
  , children :: Vector Artist
  } deriving Generic

data RelatedArtistsResponse = RelatedArtistsResponse
  { artists :: Vector Artist
  } deriving Generic

$(A.deriveJSON (mkOpts "") ''Followers)
$(A.deriveJSON (mkOpts "") ''Image)
$(A.deriveJSON (mkOpts "") ''Artist)
$(A.deriveFromJSON (mkOpts "") ''TokenResponse)
$(A.deriveJSON (mkOpts "") ''RelatedArtistsResponse)
$(A.deriveJSON (mkOpts "") ''RelatedArtists)
