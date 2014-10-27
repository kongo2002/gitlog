{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import qualified Data.ByteString.Char8 as BS

import Test.HUnit                     ( (@=?) )
import Test.Framework                 ( defaultMain, testGroup, Test )
import Test.Framework.Providers.HUnit ( testCase )

import Gitlog.Parser
import Gitlog.Types


main :: IO ()
main =
  defaultMain tests


baseEntry :: BS.ByteString -> GitEntry
baseEntry title =
  GitEntry
    "3e51aa9"
    "kongo2002"
    "2014-06-30 00:55:16 +0200"
    title
    []


tests :: [Test]
tests =
  [ testGroup "parsing"
    [ testCase "simple log entry" ([baseEntry "test"] @=? parseInput "|3e51aa9|kongo2002|2014-06-30 00:55:16 +0200|test")
    , testCase "multiple entries" ([baseEntry "test", baseEntry "foo"] @=? parseInput "|3e51aa9|kongo2002|2014-06-30 00:55:16 +0200|test\n\n|3e51aa9|kongo2002|2014-06-30 00:55:16 +0200|foo")
    , testCase "no entry" ([] @=? parseInput "")
    , testCase "invalid input" ([] @=? parseInput "invalid input")
    ]
  ]
