module Gitlog.Types where

import qualified Data.ByteString.Char8 as BS

data GitEntry = GitEntry
  { gSHA   :: BS.ByteString
  , gTitle :: BS.ByteString
  , gBody  :: [GitBody]
  } deriving ( Show, Eq, Ord )


data GitBody =
    Intern
  | Line BS.ByteString
  | Tag BS.ByteString Int
  deriving ( Show, Eq, Ord )


-- vim: set et sw=2 sts=2 tw=80:
