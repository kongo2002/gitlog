{-# LANGUAGE OverloadedStrings #-}

module Gitlog.Encoder
  ( toHtml
  ) where

import           Prelude hiding ( lines )

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.ByteString.Lazy.Builder
import           Data.Monoid    ( Monoid, mappend, mempty )

import           Gitlog.Types


toHtml :: [GitEntry] -> LBS.ByteString
toHtml = toLazyByteString . encodeHtml


encodeHtml :: [GitEntry] -> Builder
encodeHtml [] = header <> footer
encodeHtml es =
  header <>
  enc "div" "entries" (
    foldr go mempty es) <>
  footer
 where
  go x acc = entry x <> acc


entry :: GitEntry -> Builder
entry e =
  enc "div" "entry" (
    enc "div" "header" (
      enc "div" "sha" (
        byteString $ gSHA e
        ) <>
      enc "div" "author" (
        byteString $ gAuthor e
        ) <>
      enc "div" "date" (
        byteString $ gDate e
        )
      ) <>
    enc "div" "title" (
      byteString $ gTitle e
      ) <>
    lines body <>
    enc "div" "tags" (tags body))
 where
  body = gBody e

  lines [] = mempty
  lines ls = enc "div" "body" (foldr lines' mempty ls)

  lines' (Line l) a = enc "div" "line" (byteString l) <> a
  lines' _        a = a

  tags [] = mempty
  tags ts = enc "ul" "tags" $ foldr tags' mempty ts

  tags' (Tag t no _) a = enc "li" "tag" (fmt t no) <> a
  tags' _          a = a

  fmt t no =
    let tag = BS.unpack t ++ "-" ++ show no
        url = "http://localhost:9999/browse/" ++ tag
    in enc' "href" "a" url (stringUtf8 tag)


enc :: String -> String -> Builder -> Builder
enc = enc' "class"


enc' :: String -> String -> String -> Builder -> Builder
enc' attr tag v builder =
  charUtf8 '<' <> tag' <> charUtf8 ' ' <> s attr <> s "=\"" <> s v <> s "\">" <>
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
  stringUtf8 "<style type=\"text/css\">\
      \.entry{margin:5px 10px 20px 10px;}\
      \.sha{float:left;}\
      \.header{color:#999;}\
      \.author{float:left;padding-left:5em;}\
      \.date{float:left;padding-left:5em;}\
      \.title{clear:both;padding-left:2em;padding-top:0.3em;font-weight:bold;}\
      \.body{padding-left:2em;padding-top:0.5em;}\
      \.ul.tags{margin:0;}\
    \</style>"


footer :: Builder
footer =
  stringUtf8 "</body></html>"


infixr 4 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
{-# INLINE (<>) #-}


-- vim: set et sw=2 sts=2 tw=80:
