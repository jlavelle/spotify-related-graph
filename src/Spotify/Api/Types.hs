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
  deriving newtype (A.FromJSON, ToField)
  deriving Generic

instance Wrapped Url

newtype SpotifyId = SpotifyId Text
  deriving newtype (A.FromJSON, ToField, FromField, Eq, Ord)
  deriving Generic

instance Wrapped SpotifyId

newtype SpotifyUri = SpotifyUri Text
  deriving newtype (A.FromJSON, ToField)
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
  { _artistExternalUrls :: Map Text Url
  , _artistFollowers    :: Followers
  , _artistGenres       :: Vector Text
  , _artistHref         :: Url
  , _artistId           :: SpotifyId
  , _artistImages       :: Vector Image
  , _artistName         :: Text
  , _artistPopularity   :: Int
  , _artistUri          :: SpotifyUri
  }

data Followers = Followers
  { _followersHref  :: Maybe Url
  , _followersTotal :: Int
  }

data Image = Image
  { _imageHeight :: Int
  , _imageWidth  :: Int
  , _imageUrl    :: Url
  }

data TokenResponse = TokenResponse
  { _tokenResponseAccessToken :: Token
  , _tokenResponseTokenType   :: Text
  , _tokenResponseExpiresIn   :: Int
  }

data ArtistsResponse = ArtistsResponse
  { _artistsResponseArtists :: Vector Artist
  }

$(A.deriveFromJSON (mkOpts "_followers") ''Followers)
$(A.deriveFromJSON (mkOpts "_image") ''Image)
$(A.deriveFromJSON (mkOpts "_artist") ''Artist)
$(A.deriveFromJSON (mkOpts "_tokenResponse") ''TokenResponse)
$(A.deriveFromJSON (mkOpts "_artistsResponse") ''ArtistsResponse)
