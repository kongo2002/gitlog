{-# LANGUAGE OverloadedStrings #-}

module Gitlog.Encoder
  ( toHtml
  ) where

import qualified Data.ByteString.Lazy as LBS
import           Data.ByteString.Lazy.Builder
import           Data.Monoid ( Monoid, mappend )

import Gitlog.Types


toHtml :: [GitEntry] -> LBS.ByteString
toHtml = toLazyByteString . encodeHtml


encodeHtml :: [GitEntry] -> Builder
encodeHtml [] = header <> footer
encodeHtml es =
  header <>
  enc "div" "entries" (
    foldr go (stringUtf8 "</div>") (tail es)) <>
  footer
 where
  go x acc = entry x <> acc


entry :: GitEntry -> Builder
entry e =
  enc "div" "entry" (
    enc "div" "sha" (
      byteString $ gSHA e
      ) <>
    enc "div" "title" (
      byteString $ gTitle e
      )
    )


enc :: String -> String -> Builder -> Builder
enc tag cls builder =
  charUtf8 '<' <> tag' <> stringUtf8 " class=\"" <> stringUtf8 cls <> stringUtf8 "\">" <>
  builder <>
  stringUtf8 "</" <> tag' <> charUtf8 '>'
 where
  tag' = stringUtf8 tag


header :: Builder
header =
  stringUtf8 "<html><head><title>gitlog</title></head><body>"


footer :: Builder
footer =
  stringUtf8 "</body></html>"


infixr 4 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
{-# INLINE (<>) #-}
