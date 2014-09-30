{-# LANGUAGE OverloadedStrings #-}

module Gitlog.Types
  ( GitEntry(..)
  , GitBody(..)
  , Config(..)
  , JiraIssue(..)
  , defaultConfig
  , hasJira
  ) where

import           Control.Applicative
import           Control.Monad   ( mzero )
import           Control.Parallel.Strategies
import           Data.Aeson
import qualified Data.ByteString.Char8 as BS
import           Data.Time.Clock ( UTCTime )
import           Data.Maybe      ( isJust )
import           Data.Text       ( Text )
import qualified Data.Vector as V


data GitEntry = GitEntry
  { gSHA    :: BS.ByteString
  , gAuthor :: BS.ByteString
  , gDate   :: BS.ByteString
  , gTitle  :: BS.ByteString
  , gBody   :: [GitBody]
  } deriving ( Eq, Ord )


instance NFData GitEntry


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
  { jKey           :: Text
  , jSummary       :: Text
  , jToTest        :: Bool
  , jDocumentation :: Bool
  , jPR            :: Bool
  } deriving ( Eq, Ord, Show )


instance FromJSON JiraIssue where
  parseJSON (Object v) = do
    key     <- v .: "key"
    fields  <- v .: "fields"
    (s, tt, doc, pr) <- summary fields
    return $ JiraIssue key s tt doc pr
   where
    summary (Object x) = do
      s  <- x .: "summary"
      -- "to be tested"
      tt <- exists =<< (x .: "customfield_10411")
      -- "documentation relevant" and "PR relevant"
      cs <- toList =<< (x .: "customfield_10412")
      return (s, tt, hasField "10123" cs, hasField "10220" cs)
    summary _ = mzero

    exists Null = return False
    exists _    = return True

    hasField i = any ((== i) . _id)

    toList (Array xs) = mapM parseJSON $ V.toList xs
    toList _          = return []

  parseJSON _ = mzero


data CustomField = CI
  { _id    :: Text
  , _self  :: Text
  , _value :: Text
  }


instance FromJSON CustomField where
  parseJSON (Object o) =
    CI <$> o .: "id"
       <*> o .: "self"
       <*> o .: "value"
  parseJSON _ = mzero


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
