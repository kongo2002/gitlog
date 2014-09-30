module Gitlog.Utils
  ( split
  ) where


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


-- vim: set et sw=2 sts=2 tw=80:
