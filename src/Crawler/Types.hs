module Crawler.Types where

import Protolude

import Control.Concurrent.STM.TQueue (TQueue, newTQueueIO, readTQueue, writeTQueue)
import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, modifyTVar)
import Database.SQLite.Simple (Connection)
import qualified Database.SQLite.Simple as Sql
import qualified Network.HTTP.Req as Req
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Types as Http
import qualified Data.Attoparsec.Text as A
import qualified Data.List as List
import Data.Aeson (FromJSON)
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import Data.Vector (Vector)
import Control.Lens ((^.))
import Data.Generics.Product (field)

import Spotify.Api.Types
import qualified Spotify.Api as Api
import qualified Spotify.Api.Class as Api
import qualified Cache
import qualified Cache.Class as Cache

data Config = Config
  { visited     :: TVar (Set SpotifyId)
  , idQueue     :: TQueue SpotifyId
  , cacheQueue  :: TQueue (SpotifyId, Artist)
  , rateLock    :: MVar ()
  , connection  :: Connection
  , crawlers    :: Int
  , cachers     :: Int
  , credentials :: Credentials
  }

newtype CrawlerM a = CrawlerM (ReaderT Config IO a)
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader Config
           , MonadIO
           )

instance Req.MonadHttp CrawlerM where
  handleHttpException = throwIO

instance Cache.MonadCache CrawlerM where
  cacheArtists as        = mkCacheFn (\conn -> Sql.withTransaction conn $ Cache.insertArtists conn as)
  lookupArtists ids      = mkCacheFn (\conn -> Sql.withTransaction conn $ Cache.selectArtists conn ids)
  cachedIds              = mkCacheFn Cache.selectArtistIds
  cachedRelations        = mkCacheFn Cache.selectArtistRelations
  lookupDepthSearched i  = mkCacheFn (flip Cache.selectArtistDepthSearched i)
  cacheDepthSearched i n = mkCacheFn (\conn -> Sql.withTransaction conn $ Cache.upsertArtistDepthSearched conn i n)

instance Api.MonadSpotify CrawlerM where
  getAuthToken = fmap accessToken . apiCall . Api.postCredentials
  getArtist = undefined

-- lifts a request, automatically handling rate limit retrying.
apiCall :: FromJSON a => Req.Req (Req.JsonResponse a) -> CrawlerM a
apiCall res = do
  rl <- asks rateLock
  _  <- liftIO $ readMVar rl
  er <- liftIO $ try $ runReq res
  case er of
   Right a -> pure a
   Left e@(Req.VanillaHttpException (Http.HttpExceptionRequest _ (Http.StatusCodeException r _))) -> case Http.responseStatus r of
     Http.Status 429 _ -> do
       mmv <- liftIO $ tryTakeMVar rl
       case mmv of
         Nothing -> apiCall res
         Just _  -> do
           case List.lookup "Retry-After" (Http.responseHeaders r) >>= parseInt of
             Nothing -> throwIO e
             Just wt -> liftIO $ threadDelayS $ wt + 1
           liftIO $ putMVar rl ()
           apiCall res
   Left e -> throwIO e

mkCacheFn :: (Connection -> IO a) -> CrawlerM a
mkCacheFn fn = asks connection >>= liftIO . fn

runCrawlerM :: CrawlerM a -> Config -> IO a
runCrawlerM (CrawlerM rt) = runReaderT rt

forkCrawlerM :: CrawlerM () -> CrawlerM ThreadId
forkCrawlerM x = do
  cfg <- ask
  liftIO $ forkIO $ runCrawlerM x cfg

runReq :: (FromJSON a, MonadIO m) => Req.Req (Req.JsonResponse a) -> m a
runReq = fmap Req.responseBody . Req.runReq Req.defaultHttpConfig

threadDelayS :: Int -> IO ()
threadDelayS s = threadDelay $ s * 1000000

parseInt :: ByteString -> Maybe Int
parseInt = either (const Nothing) Just . A.parseOnly A.decimal . toS
