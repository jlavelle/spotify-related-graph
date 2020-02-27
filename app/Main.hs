module Main where

import Protolude hiding (getField)

import qualified Data.ByteString.Base64 as Base64
import Data.Profunctor (dimap)
import qualified System.Environment
import qualified Data.Text as T
import Data.Generics.Product (getField)
import qualified Data.List.NonEmpty as NE
import qualified Network.Wai.Handler.Warp as Warp

import Spotify
  ( Credentials(..)
  , parseArtist
  , SearchParams(..)
  , SearchType(..)
  , search
  )
import App (AppM, initConfig, app)

testSearchApi :: AppM ()
testSearchApi = do
 r <- search $ SearchParams "big black" (NE.fromList [SArtist]) 20 0
 traverse_ (traverse_ (print . getField @"name" . parseArtist) . getField @"items") (getField @"artists" r)

main :: IO ()
main = do
  Just clientId <- lookupEnv "SPOTIFY_GRAPH_CLIENT_ID"
  Just clientSecret <- lookupEnv "SPOTIFY_GRAPH_CLIENT_SECRET"
  let cs = Credentials $ Base64.encodeBase64' $ toS (clientId <> ":" <> clientSecret)
  cfg <- initConfig cs
  --runAppM testSearchApi cfg
  Warp.run 3000 $ app cfg

lookupEnv :: Text -> IO (Maybe Text)
lookupEnv =
  dimap
    T.unpack
    ((fmap . fmap) T.pack)
    System.Environment.lookupEnv
