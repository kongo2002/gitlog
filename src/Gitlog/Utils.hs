module Gitlog.Utils
  ( split
  , startsWith
  , getVersion
  , (<>)
  ) where

import Data.Monoid  ( Monoid, mappend )
import Data.List    ( stripPrefix )
import Data.Version ( showVersion )

import Paths_gitlog ( version )


------------------------------------------------------------------------------
-- | Split a string on a specific character
split :: Char -> String -> [String]
split c str =
  case dropWhile predicate str of
    [] -> []
    x  -> w : split c s'
     where (w, s') = break predicate x
 where
  predicate = (== c)


------------------------------------------------------------------------------
-- | Determine whether a list starts with a given sublist
startsWith :: Eq a => [a] -> [a] -> Bool
startsWith prefix input =
  case stripPrefix prefix input of
    (Just _) -> True
    _        -> False


------------------------------------------------------------------------------
-- | Get the string representation of the version stored in
-- the 'gitlog' cabal file
getVersion :: String
getVersion =
  showVersion version


infixr 4 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
{-# INLINE (<>) #-}


-- vim: set et sw=2 sts=2 tw=80:
