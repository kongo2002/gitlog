{-# LANGUAGE OverloadedStrings #-}

module Gitlog.Encoder.Json
  ( toJson
  ) where

import           Data.Aeson ( encode )
import qualified Data.ByteString.Lazy as LBS

import           Gitlog.Types


toJson :: [GitEntry] -> LBS.ByteString
toJson = encode


-- vim: set et sw=2 sts=2 tw=80:
