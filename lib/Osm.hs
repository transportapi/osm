-- |
-- Module    : Streaming.Osm
-- Copyright : (c) Azavea, 2017 - 2020
-- License   : BSD3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>
--
-- This library provides the ability to read and process <http://www.openstreetmap.org/ OpenStreetMap>
-- data via the <https://hackage.haskell.org/package/streaming streaming> ecosystem. Since /streaming/
-- allows for very little RAM overhead despite file size, we can process very large OSM PBF files
-- just by providing a file path:
-- @

module Osm
  ( parseOSM
  , parseBlob
  ) where

import           Control.Applicative (many)
import           Codec.Compression.Zlib (decompress)
import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import           Osm.Internal.Parser
import           Osm.Types

parseOSM :: BS.ByteString -> Either String [Blob]
parseOSM bs = A.parseOnly (many (header *> blob)) bs

parseBlob :: Blob -> Either String Block
parseBlob (Blob (Left bs)) = A.parseOnly block bs
parseBlob (Blob (Right (_, bs))) = A.parseOnly block (BL.toStrict $ decompress $ BL.fromStrict bs)
