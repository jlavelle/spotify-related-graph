{-# LANGUAGE DataKinds #-}

module Spotify.Api where

import Protolude

import Control.Lens (review, _Unwrapped', Unwrapped, Wrapped)
import qualified Network.HTTP.Req as Req
import Network.HTTP.Req (MonadHttp, (=:), (/:))

import Spotify.Api.Types (TokenResponse, Artist, Token, Credentials, RelatedArtistsResponse, SpotifyId)

rootUrl :: Req.Url 'Req.Https
rootUrl = Req.https "api.spotify.com" /: "v1"

postCredentials :: MonadHttp m => Credentials -> m (Req.JsonResponse TokenResponse)
postCredentials cs = Req.req Req.POST url body Req.jsonResponse opts
  where
    url  = Req.https "accounts.spotify.com" /: "api" /: "token"
    body = Req.ReqBodyUrlEnc $ "grant_type" =: ("client_credentials" :: Text)
    opts = Req.header "Authorization" $ "Basic " <> unwrap cs

getArtist :: MonadHttp m => SpotifyId -> Token -> m (Req.JsonResponse Artist)
getArtist id t = Req.req Req.GET url Req.NoReqBody Req.jsonResponse opts
  where
    url  = rootUrl /: "artists" /: unwrap id
    opts = tokenHeader t

getRelatedArtists :: MonadHttp m => SpotifyId -> Token -> m (Req.JsonResponse RelatedArtistsResponse)
getRelatedArtists id t = Req.req Req.GET url Req.NoReqBody Req.jsonResponse opts
  where
    url  = rootUrl /: "artists" /: unwrap id /: "related-artists"
    opts = tokenHeader t

tokenHeader :: Token -> Req.Option a
tokenHeader t = Req.header "Authorization" $ "Bearer " <> toS (unwrap t)

unwrap :: Wrapped s => s -> Unwrapped s
unwrap = review _Unwrapped'
