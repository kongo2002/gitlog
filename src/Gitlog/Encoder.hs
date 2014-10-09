module Gitlog.Encoder
  ( encode
  ) where

import qualified Data.ByteString.Lazy as LBS

import           Gitlog.Encoder.Html
import           Gitlog.Encoder.Json
import           Gitlog.Types


-- | Encode the list of @GitEntry@ depending on the configured
-- output format into HTML or JSON
encode :: Config -> [GitEntry] -> LBS.ByteString
encode cfg es =
  case cOutput cfg of
    Html -> toHtml cfg es
    Json -> toJson es


-- vim: set et sw=2 sts=2 tw=80:
