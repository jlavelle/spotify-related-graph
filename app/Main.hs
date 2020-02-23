module Main where

import Protolude

import Crawler
import Spotify.Api.Types

main :: IO ()
main = do
  cfg <- initConfig
  loadArtist cfg $ SpotifyId "5EYkvHZuGM3pwU3DZUrrZ3"
  forkIO $ mkCrawler cfg
  mkCacher cfg
