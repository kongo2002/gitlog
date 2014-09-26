{-# LANGUAGE OverloadedStrings #-}

module Gitlog.Types where

import           Control.Monad ( mzero )
import           Data.Aeson
import qualified Data.ByteString.Char8 as BS
import           Data.Time.Clock ( UTCTime )
import           Data.Text       ( Text )
import           Data.Maybe      ( isJust )


data GitEntry = GitEntry
  { gSHA    :: BS.ByteString
  , gAuthor :: BS.ByteString
  , gDate   :: BS.ByteString
  , gTitle  :: BS.ByteString
  , gBody   :: [GitBody]
  } deriving ( Eq, Ord )


data GitBody =
    Intern
  | Line BS.ByteString
  | Tag BS.ByteString Int (Maybe JiraIssue)
  deriving ( Eq, Ord )


data Config = Config
  { cRange :: Maybe (String, String)
  , cPath  :: FilePath
  , cDate  :: UTCTime
  , cJira  :: String
  , cAuth  :: Maybe (BS.ByteString, BS.ByteString)
  } deriving ( Eq, Ord, Show )


data JiraIssue = JiraIssue
  { jKey     :: Text
  , jSummary :: Text
  } deriving ( Eq, Ord, Show )


instance FromJSON JiraIssue where
  parseJSON (Object v) = do
    key    <- v .: "key"
    fields <- v .: "fields"
    summ   <- summary fields
    return $ JiraIssue key summ
   where
    summary (Object x) = x .: "summary"
    summary _ = mzero

  parseJSON _          = mzero


defaultConfig :: UTCTime -> Config
defaultConfig d = Config
  { cRange = Nothing
  , cPath  = "."
  , cDate  = d
  , cJira  = []
  , cAuth  = Nothing
  }


hasJira :: Config -> Bool
hasJira cfg =
  not (null jira) && hasAuth
 where
  jira    = cJira cfg
  hasAuth = isJust $ cAuth cfg


-- vim: set et sw=2 sts=2 tw=80:
