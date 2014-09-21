{-# LANGUAGE OverloadedStrings #-}

module Gitlog.Encoder
  ( toHtml
  ) where

import qualified Data.ByteString.Lazy as LBS
import           Data.ByteString.Lazy.Builder
import           Data.Monoid ( Monoid, mappend, mempty )

import Gitlog.Types


toHtml :: [GitEntry] -> LBS.ByteString
toHtml = toLazyByteString . encodeHtml


encodeHtml :: [GitEntry] -> Builder
encodeHtml [] = header <> footer
encodeHtml es =
  header <>
  enc "div" "entries" (
    foldr go (stringUtf8 mempty) (tail es)) <>
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
  charUtf8 '<' <> tag' <> s " class=\"" <> s cls <> s "\">" <>
  builder <>
  s "</" <> tag' <> charUtf8 '>'
 where
  tag' = stringUtf8 tag
  s = stringUtf8
  {-# INLINE s #-}


header :: Builder
header =
  stringUtf8 "<!DOCTYPE html><html><head><title>gitlog</title>" <>
  css <>
  stringUtf8 "</head><body>"


css :: Builder
css =
  stringUtf8 "<style type=\"text/css\">.entry{padding:5px 10px;float:none;}.sha{float:left;}.title{padding-left:8em;}</style>"


footer :: Builder
footer =
  stringUtf8 "</body></html>"


infixr 4 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
{-# INLINE (<>) #-}


-- vim: set et sw=2 sts=2 tw=80:
