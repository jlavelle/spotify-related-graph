{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Finite.Orphans where

import Protolude

import GHC.TypeNats (KnownNat)
import Data.Finite (Finite, packFinite)
import Servant.API (FromHttpApiData(..))
import qualified Data.Attoparsec.Text as A
import Data.Bifunctor (first)
import Data.Profunctor (lmap)

instance KnownNat n => FromHttpApiData (Finite n) where
  parseUrlPiece = parseFinite
  parseHeader = lmap toS parseFinite
  parseQueryParam = parseFinite

parseFinite :: forall n. KnownNat n => Text -> Either Text (Finite n)
parseFinite t =
      first toS (A.parseOnly A.decimal t)
  >>= maybe (Left "Input too large") Right . packFinite @n
