{-# LANGUAGE RecordWildCards #-}

module Crawler where

import Protolude

import qualified System.Environment
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Text as T
import Data.Profunctor (dimap)
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

initConfig :: IO Config
initConfig = do
  Just clientId <- lookupEnv "SPOTIFY_GRAPH_CLIENT_ID"
  Just clientSecret <- lookupEnv "SPOTIFY_GRAPH_CLIENT_SECRET"
  let credentials = Credentials $ Base64.encodeBase64' $ toS (clientId <> ":" <> clientSecret)
  connection <- Sql.open "cache.db"
  Cache.setupDb connection
  visited <- Cache.selectArtistIds connection >>= atomically . newTVar
  idQueue <- newTQueueIO
  cacheQueue <- newTQueueIO
  rateLock <- newMVar ()
  let crawlers = 1
      cachers  = 1
  pure Config{..}

loadArtist :: Config -> SpotifyId -> IO ()
loadArtist Config{..} id = do
  t <- getToken credentials
  a <- runReq $ Api.getArtist t id
  v <- atomically $ readTVar visited
  unless (Set.member (a ^. field @"id") v) $ Cache.insertArtist connection a
  atomically do
    writeTQueue idQueue $ a ^. field @"id"
    modifyTVar visited $ Set.insert $ a ^. field @"id"

mkCacher :: Config -> IO ()
mkCacher Config{..} = loop
  where
    loop = do
      (id, a) <- atomically $ readTQueue cacheQueue
      Sql.withTransaction connection do
        Cache.insertArtist connection a
        Cache.insertArtistRelation connection id $ a ^. field @"id"
      loop

mkCrawler :: Config -> IO ()
mkCrawler Config{..} = do
  i <- atomically $ readTQueue idQueue
  t <- getToken credentials
  loop i t
  where
    loop id token = do
      _  <- readMVar rateLock
      es <- try $ runReq $ Api.getRelatedArtists token id
      case es of
        Right as -> do
          vs <- atomically $ readTVar visited
          for_ (unvisited vs as) \a -> atomically do
            modifyTVar visited $ Set.insert $ a ^. field @"id"
            writeTQueue cacheQueue (id, a)
            writeTQueue idQueue $ a ^. field @"id"
          id' <- atomically $ readTQueue idQueue
          loop id' token
        Left (Req.VanillaHttpException e) -> case e of
          Http.HttpExceptionRequest _ c -> case c of
            Http.StatusCodeException r _ -> case Http.responseStatus r of
              Http.Status 401 _ -> getToken credentials >>= loop id
              Http.Status 429 _ -> do
                mmv <- tryTakeMVar rateLock
                case mmv of
                  Nothing -> loop id token
                  Just _ -> do
                    let mwt = List.lookup "Retry-After" (Http.responseHeaders r) >>= parseInt
                    case mwt of
                      Just wt -> threadDelayS $ wt + 1
                      Nothing -> throwIO e
                    putMVar rateLock ()
                    loop id token
              _ -> throwIO e
            _ -> throwIO e
          Http.InvalidUrlException _ _ -> throwIO e
        Left e -> throwIO e

unvisited :: Set SpotifyId -> ArtistsResponse -> Vector Artist
unvisited v = Vector.filter (not . (`Set.member` v) . id) . artists

lookupEnv :: Text -> IO (Maybe Text)
lookupEnv =
  dimap
    T.unpack
    ((fmap . fmap) T.pack)
    System.Environment.lookupEnv

getToken :: Credentials -> IO Token
getToken c = accessToken <$> runReq (Api.postCredentials c)

runReq :: (FromJSON a, MonadIO m) => Req.Req (Req.JsonResponse a) -> m a
runReq = fmap Req.responseBody . Req.runReq Req.defaultHttpConfig

threadDelayS :: Int -> IO ()
threadDelayS s = threadDelay $ s * 1000000

parseInt :: ByteString -> Maybe Int
parseInt = either (const Nothing) Just . A.parseOnly A.decimal . toS
