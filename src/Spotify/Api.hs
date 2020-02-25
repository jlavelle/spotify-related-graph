module Spotify.Api where

import Protolude hiding (to)

import Control.Lens (review, _Unwrapped', Unwrapped, Wrapped, (^.), to)
import qualified Network.HTTP.Req as Req
import Network.HTTP.Req (MonadHttp, (=:), (/:))
import Data.Generics.Product (field)
import qualified Data.Text as T

import Spotify.Api.Types
  ( TokenResponse
  , Credentials
  , Artist
  , SpotifyId
  , Token
  , RelatedArtistsResponse
  , SearchParams
  , SearchResponse
  , SearchType(..)
  )

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

getSearch :: MonadHttp m => SearchParams -> Token -> m (Req.JsonResponse SearchResponse)
getSearch ps t = Req.req Req.GET url Req.NoReqBody Req.jsonResponse opts
  where
    url  = rootUrl /: "search"
    opts =
         "q"      =: ps ^. field @"query"
      <> "type"   =: ps ^. field @"searchType" ^. to printSearchType
      <> "limit"  =: ps ^. field @"limit"
      <> "offset" =: ps ^. field @"offset"
      <> tokenHeader t

printSearchType :: NonEmpty SearchType -> Text
printSearchType = T.intercalate "," . toList . fmap showst
  where
    showst = \case
      SAlbum    -> "album"
      SArtist   -> "artist"
      STrack    -> "track"
      SPlaylist -> "playlist"

tokenHeader :: Token -> Req.Option a
tokenHeader t = Req.header "Authorization" $ "Bearer " <> toS (unwrap t)

unwrap :: Wrapped s => s -> Unwrapped s
unwrap = review _Unwrapped'
