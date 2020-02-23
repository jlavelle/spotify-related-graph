{-# LANGUAGE RecordWildCards #-}

module Crawler where

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
import qualified Cache

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

runCrawlerM :: CrawlerM a -> Config -> IO a
runCrawlerM (CrawlerM rt) = runReaderT rt

forkCrawlerM :: CrawlerM () -> CrawlerM ThreadId
forkCrawlerM x = do
  cfg <- ask
  liftIO $ forkIO $ runCrawlerM x cfg

type MonadCrawler m = (MonadReader Config m, Req.MonadHttp m)

initConfig :: Credentials -> IO Config
initConfig credentials = do
  connection <- Sql.open "cache.db"
  Cache.setupDb connection
  visited <- Cache.selectArtistIds connection >>= atomically . newTVar
  idQueue <- newTQueueIO
  cacheQueue <- newTQueueIO
  rateLock <- newMVar ()
  let crawlers = 1
      cachers  = 1
  pure Config{..}

loadArtist :: MonadCrawler m => SpotifyId -> m ()
loadArtist id = do
  Config{..} <- ask
  t <- liftIO $ getToken credentials
  a <- runReq $ Api.getArtist t id
  v <- liftIO $ atomically $ readTVar visited
  unless (Set.member (a ^. field @"id") v) $ liftIO $ Cache.insertArtist connection a
  liftIO $ atomically do
    writeTQueue idQueue $ a ^. field @"id"
    modifyTVar visited $ Set.insert $ a ^. field @"id"

mkCacher :: MonadCrawler m => m ()
mkCacher = loop
  where
    loop = do
      Config{..} <- ask
      (id, a) <- liftIO $ atomically $ readTQueue cacheQueue
      liftIO $ Sql.withTransaction connection do
        Cache.insertArtist connection a
        Cache.insertArtistRelation connection id $ a ^. field @"id"
      loop

mkCrawler :: forall m. MonadCrawler m => m ()
mkCrawler = do
  Config{..} <- ask
  i <- liftIO $ atomically $ readTQueue idQueue
  t <- liftIO $ getToken credentials
  loop i t
  where
    loop :: SpotifyId -> Token -> m ()
    loop id token = do
      Config {..} <- ask
      _  <- liftIO $ readMVar rateLock
      es <- liftIO $ try $ runReq $ Api.getRelatedArtists token id
      case es of
        Right as -> do
          vs <- liftIO $ atomically $ readTVar visited
          liftIO $ for_ (unvisited vs as) \a -> atomically do
            modifyTVar visited $ Set.insert $ a ^. field @"id"
            writeTQueue cacheQueue (id, a)
            writeTQueue idQueue $ a ^. field @"id"
          id' <- liftIO $ atomically $ readTQueue idQueue
          loop id' token
        Left (Req.VanillaHttpException e) -> case e of
          Http.HttpExceptionRequest _ c -> case c of
            Http.StatusCodeException r _ -> case Http.responseStatus r of
              Http.Status 401 _ -> liftIO (getToken credentials) >>= loop id
              Http.Status 429 _ -> do
                mmv <- liftIO $ tryTakeMVar rateLock
                case mmv of
                  Nothing -> loop id token
                  Just _ -> do
                    let mwt = List.lookup "Retry-After" (Http.responseHeaders r) >>= parseInt
                    case mwt of
                      Just wt -> liftIO $ threadDelayS $ wt + 1
                      Nothing -> throwIO e
                    liftIO $ putMVar rateLock ()
                    loop id token
              _ -> throwIO e
            _ -> throwIO e
          Http.InvalidUrlException _ _ -> throwIO e
        Left e -> throwIO e

unvisited :: Set SpotifyId -> ArtistsResponse -> Vector Artist
unvisited v = Vector.filter (not . (`Set.member` v) . id) . artists

getToken :: Credentials -> IO Token
getToken c = accessToken <$> runReq (Api.postCredentials c)

runReq :: (FromJSON a, MonadIO m) => Req.Req (Req.JsonResponse a) -> m a
runReq = fmap Req.responseBody . Req.runReq Req.defaultHttpConfig

threadDelayS :: Int -> IO ()
threadDelayS s = threadDelay $ s * 1000000

parseInt :: ByteString -> Maybe Int
parseInt = either (const Nothing) Just . A.parseOnly A.decimal . toS
