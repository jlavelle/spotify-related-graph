module Main where

import Protolude

import qualified Data.ByteString.Base64 as Base64
import Data.Profunctor (dimap)
import qualified System.Environment
import qualified Data.Text as T

import Crawler
import Spotify.Api.Types

crawl :: CrawlerM ()
crawl = do
  loadArtist $ SpotifyId "5EYkvHZuGM3pwU3DZUrrZ3"
  _ <- forkCrawlerM mkCrawler
  mkCacher

main :: IO ()
main = do
  Just clientId <- lookupEnv "SPOTIFY_GRAPH_CLIENT_ID"
  Just clientSecret <- lookupEnv "SPOTIFY_GRAPH_CLIENT_SECRET"
  let cs = Credentials $ Base64.encodeBase64' $ toS (clientId <> ":" <> clientSecret)
  cfg <- initConfig cs
  runCrawlerM crawl cfg

lookupEnv :: Text -> IO (Maybe Text)
lookupEnv =
  dimap
    T.unpack
    ((fmap . fmap) T.pack)
    System.Environment.lookupEnv
