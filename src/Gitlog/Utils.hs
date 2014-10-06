module Gitlog.Utils
  ( split
  , startsWith
  ) where

import Data.List ( stripPrefix )


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


-- vim: set et sw=2 sts=2 tw=80:
