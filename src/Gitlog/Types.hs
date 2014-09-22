module Gitlog.Types where

import qualified Data.ByteString.Char8 as BS
import           Data.Time.Clock ( UTCTime )


data GitEntry = GitEntry
  { gSHA    :: BS.ByteString
  , gAuthor :: BS.ByteString
  , gDate   :: BS.ByteString
  , gTitle  :: BS.ByteString
  , gBody   :: [GitBody]
  } deriving ( Show, Eq, Ord )


data GitBody =
    Intern
  | Line BS.ByteString
  | Tag BS.ByteString Int String
  deriving ( Show, Eq, Ord )


data Config = Config
  { cRange :: Maybe (String, String)
  , cPath  :: FilePath
  , cDate  :: UTCTime
  , cJira  :: String
  } deriving ( Eq, Ord )


defaultConfig :: UTCTime -> Config
defaultConfig d = Config
  { cRange = Nothing
  , cPath  = "."
  , cDate  = d
  , cJira  = []
  }


-- vim: set et sw=2 sts=2 tw=80:
