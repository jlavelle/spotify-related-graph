{-# LANGUAGE ViewPatterns    #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module App where

import Protolude hiding (getField, try)

import qualified Database.SQLite.Simple as Sql
import Data.Generics.Product (getField)
import Control.Monad.Catch (MonadCatch, MonadThrow, MonadMask, try, throwM)
import Control.Monad.Base (MonadBase)
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Data.Pool (Pool)
import qualified Data.Pool as Pool
import qualified Network.HTTP.Req as Req
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Types as Http
import qualified Data.Attoparsec.Text as A
import qualified Data.List as List

import Spotify (MonadSpotify(..), Artist, RelatedArtists(..), SpotifyId, Token, Credentials)
import qualified Spotify.Api as Api
import Cacheable (Cacheable)
import qualified Cacheable

data Config = Config
  { pool        :: Pool Sql.Connection
  , credentials :: Credentials
  , rateLock    :: MVar ()
  , token       :: MVar Token
  } deriving Generic

newtype AppM a = AppM { unAppM :: ReaderT Config IO a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader Config
           , MonadIO
           , MonadCatch
           , MonadThrow
           , MonadMask
           , MonadBase IO
           )

instance MonadBaseControl IO AppM where
  type StM AppM a = a
  liftBaseWith f = AppM $ liftBaseWith \g -> f (g . unAppM)
  restoreM = AppM . restoreM

instance Req.MonadHttp AppM where
  handleHttpException = throwM

instance MonadSpotify AppM where
  getArtist = Cacheable.withCache getArtistImpl
  getRelatedArtists = fmap (getField @"children") . Cacheable.withCache' (getField @"id") getRelatedArtistsImpl

-- TODO Just use Typeable or something for the table names
instance Cacheable AppM SpotifyId Artist where
  cache  = Cacheable.mkCacheFn "Artist" (getField @"id")
  lookup = Cacheable.mkLookupFn "Artist"
  allIds = Cacheable.mkAllIds "Artist"

instance Cacheable AppM SpotifyId RelatedArtists where
  cache  = Cacheable.mkCacheFn "RelatedArtists" (getField @"parent")
  lookup = Cacheable.mkLookupFn "RelatedArtists"
  allIds = Cacheable.mkAllIds "RelatedArtists"

runAppM :: AppM a -> Config -> IO a
runAppM (unAppM -> m) c = runReaderT m c

initConfig :: Credentials -> IO Config
initConfig cs = do
  token    <- getTokenIO cs >>= newMVar
  rateLock <- newMVar ()
  pool     <- mkPool
  let credentials = cs
  pure Config{..}
  where
    mkPool = Pool.createPool (Sql.open "cache.db") Sql.close 1 10 1

-- TODO 404 Handling

getArtistImpl :: SpotifyId -> AppM Artist
getArtistImpl id = fmap Req.responseBody $ mkApiCall $ Api.getArtist id

getRelatedArtistsImpl :: Artist -> AppM RelatedArtists
getRelatedArtistsImpl (getField @"id" -> id) = fmap go $ mkApiCall $ Api.getRelatedArtists id
  where
    go = RelatedArtists id . getField @"artists" . Req.responseBody

getAccessToken :: AppM Token
getAccessToken = do
  cs <- asks credentials
  mkApiCallNoToken $ liftIO $ getTokenIO cs

getTokenIO :: Credentials -> IO Token
getTokenIO cs = Req.runReq Req.defaultHttpConfig $ getField @"accessToken" . Req.responseBody <$> Api.postCredentials cs

mkApiCall :: (Token -> AppM a) -> AppM a
mkApiCall act = do
  t <- asks token >>= liftIO . readMVar
  try (mkApiCallNoToken $ act t) >>= either handleError pure
  where
    handleError (expiredToken -> Just _) = refreshToken *> mkApiCall act
    handleError e = throwM e

mkApiCallNoToken :: AppM a -> AppM a
mkApiCallNoToken act = do
  rl <- asks rateLock
  _  <- liftIO $ readMVar rl
  try act >>= either handleError pure
  where
    handleError (rateLimited -> Just wt) = awaitRateLimit wt *> mkApiCallNoToken act
    handleError e = throwM e

awaitRateLimit :: Int -> AppM ()
awaitRateLimit wt = do
  rl <- asks rateLock
  ml <- liftIO $ tryTakeMVar rl
  case ml of
    Nothing -> pure ()
    Just _  -> threadDelayS (wt + 1) *> liftIO (putMVar rl ())

refreshToken :: AppM ()
refreshToken = do
  t  <- asks token
  mt <- liftIO $ tryTakeMVar t
  case mt of
    Nothing -> pure ()
    Just _  -> getAccessToken >>= liftIO . putMVar t

rateLimited :: Req.HttpException -> Maybe Int
rateLimited e = do
  r <- statusCodeExceptionResponse e
  case Http.responseStatus r of
    Http.Status 429 _ -> getRetryAfter r
    _                 -> Nothing

expiredToken :: Req.HttpException -> Maybe ()
expiredToken e = statusCodeExceptionResponse e <&> Http.responseStatus >>= \case
  Http.Status 401 _ -> Just ()
  _                 -> Nothing

statusCodeExceptionResponse :: Req.HttpException -> Maybe (Http.Response ())
statusCodeExceptionResponse
  (Req.VanillaHttpException
    (Http.HttpExceptionRequest _
      (Http.StatusCodeException r _))) = Just r
statusCodeExceptionResponse _ = Nothing

threadDelayS :: MonadIO m => Int -> m ()
threadDelayS s = liftIO $ threadDelay $ s * 1000000

getRetryAfter :: Http.Response a -> Maybe Int
getRetryAfter r = List.lookup "Retry-After" (Http.responseHeaders r) >>= parseInt

parseInt :: ByteString -> Maybe Int
parseInt = either (const Nothing) Just . A.parseOnly A.decimal . toS
