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


toHtml :: Config -> [GitEntry] -> LBS.ByteString
toHtml c xs = toLazyByteString $ encodeHtml c xs


encodeHtml :: Config -> [GitEntry] -> Builder
encodeHtml _ [] = header <> footer
encodeHtml c es =
  header <>
  enc "div" "content" (
    s "<h1>git log</h1>" <>
    range <>
    enc "div" "entries" (foldr go mempty es) <>
    enc "div" "footnote" (
      s "generated by " <>
      enc' "href" "a" "https://github.com/kongo2002/gitlog/" (
        s "gitlog")
      )
    ) <>
  footer
 where
  go x acc = entry c x <> acc
  commit x = enc "span" "commit" (s x)
  range =
    case cRange c of
      Nothing       -> mempty
      (Just (f, t)) ->
        enc "div" "range" (
          s "revisions from " <> commit f <> s " to " <> commit t)


entry :: Config -> GitEntry -> Builder
entry c e =
  enc "div" "entry" (
    enc "div" "header" (
      enc "div" "sha" (
        byteString $ gSHA e) <>
      enc "div" "author" (
        byteString $ gAuthor e) <>
      enc "div" "date" (
        byteString $ gDate e)
      ) <>
    enc "div" "title" (
      byteString $ gTitle e
      ) <>
    lines body <>
    enc "div" "tags" (tags body))
 where
  body   = gBody e
  jira   = cJira c
  hasUrl = not $ null jira

  lines [] = mempty
  lines ls = enc "div" "body" (foldr lines' mempty ls)

  lines' (Line l) a = enc "div" "line" (byteString l) <> a
  lines' _        a = a

  tags [] = mempty
  tags ts = enc "ul" "tags" $ foldr tags' mempty ts

  tags' (Tag t no _) a = enc "li" "tag" (fmt t no) <> a
  tags' _          a = a

  fmt t no
    | hasUrl =
      let tag = BS.unpack t ++ "-" ++ show no
          url = jira ++ "/browse/" ++ tag
      in enc' "href" "a" url (s tag)
    | otherwise = byteString t <> charUtf8 '-' <> s (show no)


enc :: String -> String -> Builder -> Builder
enc = enc' "class"


enc' :: String -> String -> String -> Builder -> Builder
enc' attr tag v builder =
  charUtf8 '<' <> tag' <> charUtf8 ' ' <> s attr <> s "=\"" <> s v <> s "\">" <>
  builder <>
  s "</" <> tag' <> charUtf8 '>'
 where
  tag' = s tag


header :: Builder
header =
  s "<!DOCTYPE html><html><head>\
      \<meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\">\
      \<title>gitlog</title>" <>
  css <>
  s "</head><body>"


css :: Builder
css =
  s "<style type=\"text/css\">\
      \ body{font-family:monospace,sans-serif;font-size:90%;}\
      \.content{width:600px;margin:auto;padding-top:2em;}\
      \.range{padding-bottom:1em;}\
      \.commit{color:#66d;}\
      \.entry{margin:5px 10px 20px 10px;}\
      \.sha{float:left;}\
      \.header{color:#999;font-size:85%;}\
      \.author{float:left;padding-left:5em;}\
      \.date{float:right;padding-right:2em;}\
      \.title{clear:both;padding-left:2em;padding-top:0.3em;font-weight:bold;}\
      \.body{padding-left:2em;padding-top:0.5em;}\
      \.footnote{font-size:60%;color:#999;border-top:1px solid #999;padding-top:8px;text-align:center;}\
      \ ul.tags{margin:0;}\
      \ a{text-decoration:none;color:#66d;}\
    \</style>"


footer :: Builder
footer = s "</body></html>"


infixr 4 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
{-# INLINE (<>) #-}


s :: String -> Builder
s = stringUtf8
{-# INLINE s #-}

-- vim: set et sw=2 sts=2 tw=80:
