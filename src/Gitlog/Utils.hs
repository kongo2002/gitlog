module Gitlog.Utils
  ( split
  , startsWith
  , getName
  , getVersion
  , (.&&.)
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
-- | Get the program name with its version string
getName :: String
getName =
  "gitlog " ++ getVersion


------------------------------------------------------------------------------
-- | Get the string representation of the version stored in
-- the 'gitlog' cabal file
getVersion :: String
getVersion =
  showVersion version


------------------------------------------------------------------------------
-- | Minor helper function to combine to filter functions
(.&&.) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(.&&.) a b x = a x && b x


infixr 4 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
{-# INLINE (<>) #-}


-- vim: set et sw=2 sts=2 tw=80:
