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

import Spotify.Api.Types
import qualified Spotify.Api as Api
import qualified Cache

data Config = Config
  { configVisited      :: TVar (Set SpotifyId)
  , configIdQueue      :: TQueue SpotifyId
  , configCacheQueue   :: TQueue (SpotifyId, Artist)
  , configRateLock     :: MVar ()
  , configConnection   :: Connection
  , configCrawlers     :: Int
  , configCachers      :: Int
  , configCredentials  :: Credentials
  }

initConfig :: IO Config
initConfig = do
  Just clientId <- lookupEnv "SPOTIFY_GRAPH_CLIENT_ID"
  Just clientSecret <- lookupEnv "SPOTIFY_GRAPH_CLIENT_SECRET"
  let configCredentials = Credentials $ Base64.encodeBase64' $ toS (clientId <> ":" <> clientSecret)
  configConnection <- Sql.open "cache.db"
  Cache.setupDb configConnection
  configVisited <- Cache.selectArtistIds configConnection >>= atomically . newTVar
  configIdQueue <- newTQueueIO
  configCacheQueue <- newTQueueIO
  configRateLock <- newMVar ()
  let configCrawlers = 1
      configCachers  = 1
  pure Config{..}

loadArtist :: Config -> SpotifyId -> IO ()
loadArtist Config{..} id = do
  t <- getToken configCredentials
  a <- runReq $ Api.getArtist t id
  v <- atomically $ readTVar configVisited
  unless (Set.member (_artistId a) v) $ Cache.insertArtist configConnection a
  atomically do
    writeTQueue configIdQueue $ _artistId a
    modifyTVar configVisited $ Set.insert $ _artistId a

mkCacher :: Config -> IO ()
mkCacher Config{..} = loop
  where
    loop = do
      (id, a) <- atomically $ readTQueue configCacheQueue
      Sql.withTransaction configConnection do
        Cache.insertArtist configConnection a
        Cache.insertArtistRelation configConnection id $ _artistId a
      loop

mkCrawler :: Config -> IO ()
mkCrawler Config{..} = do
  i <- atomically $ readTQueue configIdQueue
  t <- getToken configCredentials
  loop i t
  where
    loop id token = do
      _  <- readMVar configRateLock
      es <- try $ runReq $ Api.getRelatedArtists token id
      case es of
        Right as -> do
          visited <- atomically $ readTVar configVisited
          for_ (unvisited visited as) \a -> atomically do
            modifyTVar configVisited $ Set.insert $ _artistId a
            writeTQueue configCacheQueue (id, a)
            writeTQueue configIdQueue $ _artistId a
          id' <- atomically $ readTQueue configIdQueue
          loop id' token
        Left (Req.VanillaHttpException e) -> case e of
          Http.HttpExceptionRequest _ c -> case c of
            Http.StatusCodeException r _ -> case Http.responseStatus r of
              Http.Status 401 _ -> getToken configCredentials >>= loop id
              Http.Status 429 _ -> do
                mmv <- tryTakeMVar configRateLock
                case mmv of
                  Nothing -> loop id token
                  Just _ -> do
                    let mwt = List.lookup "Retry-After" (Http.responseHeaders r) >>= parseInt
                    case mwt of
                      Just wt -> threadDelayS $ wt + 1
                      Nothing -> throwIO e
                    putMVar configRateLock ()
                    loop id token
              _ -> throwIO e
            _ -> throwIO e
          Http.InvalidUrlException _ _ -> throwIO e
        Left e -> throwIO e

unvisited :: Set SpotifyId -> ArtistsResponse -> Vector Artist
unvisited v = Vector.filter (not . (`Set.member` v) . _artistId) . _artistsResponseArtists

lookupEnv :: Text -> IO (Maybe Text)
lookupEnv =
  dimap
    T.unpack
    ((fmap . fmap) T.pack)
    System.Environment.lookupEnv

getToken :: Credentials -> IO Token
getToken c = _tokenResponseAccessToken <$> runReq (Api.postCredentials c)

runReq :: (FromJSON a, MonadIO m) => Req.Req (Req.JsonResponse a) -> m a
runReq = fmap Req.responseBody . Req.runReq Req.defaultHttpConfig

threadDelayS :: Int -> IO ()
threadDelayS s = threadDelay $ s * 1000000

parseInt :: ByteString -> Maybe Int
parseInt = either (const Nothing) Just . A.parseOnly A.decimal . toS
