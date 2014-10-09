{-# LANGUAGE OverloadedStrings #-}

module Gitlog.Encoder.Html
  ( toHtml
  ) where

import           Prelude hiding     ( lines )

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.ByteString.Lazy.Builder
import           Data.Monoid        ( mempty )
import           Data.Text.Encoding ( encodeUtf8 )

import           Gitlog.Types
import           Gitlog.Utils


------------------------------------------------------------------------------
-- | Convert a list of @GitEntry@ into a lazy bytestring representing
-- the HTML output
toHtml :: Config -> [GitEntry] -> LBS.ByteString
toHtml c xs = toLazyByteString $ encodeHtml c xs


------------------------------------------------------------------------------
-- | Encode HTML output using a @Builder@
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


------------------------------------------------------------------------------
-- | HTML output for one @GitEntry@
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

  tags' t@(Tag{}) a = enc "li" "tag" (fmt t) <> a
  tags' _          a = a

  fmt (Tag t no ji)
    | hasUrl =
      let tag = BS.unpack t ++ "-" ++ show no
          url = jira ++ "/browse/" ++ tag
      in enc' "href" "a" url (s tag) <> jiraInfo ji
    | otherwise = byteString t <> charUtf8 '-' <> s (show no)
  fmt _ = mempty

  jiraInfo j =
    case j of
      (Just (JiraIssue _ sm test doc pr)) ->
        charUtf8 ' ' <>
        enc "span" "jira" (charUtf8 '(' <> byteString (encodeUtf8 sm) <> charUtf8 ')') <>
        when "relevant test" (s "to be tested") test <>
        when "relevant doc" (s "relevant to documentation") doc <>
        when "relevant pr" (s "relevant to PR") pr
       where
        when t name cond =
          if cond then enc "div" t name else mempty
      Nothing -> mempty


------------------------------------------------------------------------------
-- | Enclosing tag helper function for 'class' tags
enc :: String -> String -> Builder -> Builder
enc = enc' "class"


------------------------------------------------------------------------------
-- | Enclosing tag helper function
enc' :: String -> String -> String -> Builder -> Builder
enc' attr tag v builder =
  charUtf8 '<' <> tag' <> charUtf8 ' ' <> s attr <> s "=\"" <> s v <> s "\">" <>
  builder <>
  s "</" <> tag' <> charUtf8 '>'
 where
  tag' = s tag


------------------------------------------------------------------------------
-- | HTML header
header :: Builder
header =
  s "<!DOCTYPE html><html><head>\
      \<meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\">\
      \<title>gitlog</title>" <>
  css <>
  s "</head><body>"


------------------------------------------------------------------------------
-- | Default CSS definitions
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
      \.relevant{font-size:80%;padding:0.2em 0.5em;color:#a22;}\
      \ div.tags{padding-top:0.6em;}\
      \ ul.tags{margin:0;}\
      \ a{text-decoration:none;color:#66d;}\
    \</style>"


------------------------------------------------------------------------------
-- | HTML footer
footer :: Builder
footer = s "</body></html>"


s :: String -> Builder
s = stringUtf8
{-# INLINE s #-}


-- vim: set et sw=2 sts=2 tw=80:
