{-# LANGUAGE TemplateHaskell #-}

module Spotify.Api.Types where

import Protolude

import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A
import qualified Network.HTTP.Req as Req
import Data.Vector (Vector)
import Control.Lens.Wrapped (Wrapped)
import Database.SQLite.Simple.ToField (ToField)
import Database.SQLite.Simple.FromField (FromField)

import AesonUtil (mkOpts)

data Error
  = InvalidCredentials
  | ExpiredToken
  | ReqError Req.HttpException

newtype Url = Url Text
  deriving newtype (A.FromJSON, ToField, FromField)
  deriving Generic

instance Wrapped Url

newtype SpotifyId = SpotifyId Text
  deriving newtype (A.FromJSON, ToField, FromField, Eq, Ord)
  deriving Generic

instance Wrapped SpotifyId

newtype SpotifyUri = SpotifyUri Text
  deriving newtype (A.FromJSON, ToField, FromField)
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
  { followers    :: Followers
  , genres       :: Vector Text
  , href         :: Url
  , id           :: SpotifyId
  , images       :: Vector Image
  , name         :: Text
  , popularity   :: Int
  , uri          :: SpotifyUri
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

data ArtistsResponse = ArtistsResponse
  { artists :: Vector Artist
  } deriving Generic

$(A.deriveFromJSON (mkOpts "") ''Followers)
$(A.deriveFromJSON (mkOpts "") ''Image)
$(A.deriveFromJSON (mkOpts "") ''Artist)
$(A.deriveFromJSON (mkOpts "") ''TokenResponse)
$(A.deriveFromJSON (mkOpts "") ''ArtistsResponse)
