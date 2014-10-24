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
import           Data.Time.Clock    ( UTCTime(..) )
import           Data.Time.Calendar ( toGregorian )

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
    generated <>
    enc "div" "entries" (foldr go mempty es) <>
    enc "div" "footnote" (
      s "generated by " <>
      enc' "href" "a" "https://github.com/kongo2002/gitlog/" (s "gitlog"))
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
  generated = enc "div" "timestamp" (s "generated on " <> s date)
  date = simpleDate $ cDate c


------------------------------------------------------------------------------
-- | Poor man's date formatting function
simpleDate :: UTCTime -> String
simpleDate (UTCTime date diff) =
  show y ++ "-" ++ show m ++ "-" ++ show d ++ " " ++
  fmt hours ++ ":" ++ fmt minutes ++ ":" ++ fmt seconds ++ " UTC"
 where
  (y, m, d) = toGregorian date
  total     = floor $ toRational diff
  hours     = total `div` (3600 :: Int)
  totalMin  = total `mod` 3600
  minutes   = totalMin `div` 60
  seconds   = totalMin `mod` 60
  fmt x
    | x < 10    = "0" ++ show x
    | otherwise = show x


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
      escape $ gTitle e
      ) <>
    enc "div" "tags" (tags body) <>
    lines body)
 where
  body   = gBody e
  jira   = cJira c
  hasUrl = not $ null jira

  lines [] = mempty
  lines ls = enc "div" "body" (foldr lines' mempty ls)

  lines' (Line l) a = enc "div" "line" (escape l) <> a
  lines' _        a = a

  tags [] = mempty
  tags ts = enc "ul" "tags" $ foldr tags' mempty ts

  tags' t@(Tag{}) a = enc "li" "tag" (jirafmt t) <> a
  tags' _          a = a

  jirafmt (Tag t no ji) =
    jiraurl (maybe
      (t `BS.append` "-" `BS.append` BS.pack (show no))
      (encodeUtf8 . jKey)
      ji)
    <> maybe mempty jirainfo ji
  jirafmt _ = mempty

  jiraurl tag
    | hasUrl =
      let tag' = BS.unpack tag
      in  enc' "href" "a" (jira ++ "/browse/" ++ tag') (s tag')
    | otherwise = byteString tag

  jirainfo (JiraIssue _ sm test doc pr st p) =
    charUtf8 ' ' <>
    enc "span" "jira" (
      enc "span" "status" (charUtf8 '(' <> status <> charUtf8 ')') <>
      s ": " <>
      enc "span" "summary" (esc sm)) <>
    maybe mempty parent p <>
    when "relevant test" (s "to be tested") test <>
    when "relevant doc" (s "relevant to documentation") doc <>
    when "relevant pr" (s "relevant to PR") pr
   where
    esc         = escape . encodeUtf8
    status      = s $ show st
    when t n cd =
      if cd then enc "div" t n else mempty

    parent issue =
      enc "div" "parent" (
        s "Parent issue:" <>
        enc "div" [] (
          jiraurl (encodeUtf8 key) <> s " (" <> s pStat <> s "): " <>
          esc summary))
     where
      key     = jKey issue
      summary = jSummary issue
      pStat   = show $ jStatus issue


------------------------------------------------------------------------------
-- | Enclosing tag helper function for 'class' tags
enc :: String -> String -> Builder -> Builder
enc = enc' "class"


------------------------------------------------------------------------------
-- | Enclosing tag helper function
enc' :: String -> String -> String -> Builder -> Builder
enc' attr tag v builder =
  charUtf8 '<' <> tag' <> attr' <> charUtf8 '>' <>
  builder <>
  s "</" <> tag' <> charUtf8 '>'
 where
  tag' = s tag
  attr' =
    case v of
      [] -> mempty
      _  -> charUtf8 ' ' <> s attr <> s "=\"" <> s v <> charUtf8 '"'


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
      \.timestamp{padding-bottom:1em;}\
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
      \ div.tags{padding:0.6em 0;}\
      \ ul.tags{margin:0;}\
      \ a{text-decoration:none;color:#66d;}\
      \.parent{margin:0.5em;font-size:85%;}\
    \</style>"


------------------------------------------------------------------------------
-- | HTML footer
footer :: Builder
footer = s "</body></html>"


------------------------------------------------------------------------------
-- | Simple HTML string escaping
escape :: BS.ByteString -> Builder
escape str =
  case BS.uncons t of
    Nothing      -> byteString h
    Just (c, cs) -> byteString h <> escape' c <> escape cs
 where
  splitby x = x `elem` "&<>"
  (h, t)    = BS.break splitby str

  escape' '&' = s "&amp;"
  escape' '<' = s "&lt;"
  escape' '>' = s "&gt;"
  escape' c   = charUtf8 c


s :: String -> Builder
s = stringUtf8
{-# INLINE s #-}


-- vim: set et sw=2 sts=2 tw=80:
