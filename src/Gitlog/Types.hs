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


------------------------------------------------------------------------------
-- | Record describing one git commit
data GitEntry = GitEntry
  { gSHA    :: BS.ByteString
  , gAuthor :: BS.ByteString
  , gDate   :: BS.ByteString
  , gTitle  :: BS.ByteString
  , gBody   :: [GitBody]
  } deriving ( Eq, Ord, Show )


------------------------------------------------------------------------------
-- | Git commit message body
data GitBody =
    Tag BS.ByteString Int (Maybe JiraIssue)
  | Line BS.ByteString
  | Intern
  deriving ( Ord, Show )


instance NFData GitBody


instance Eq GitBody where
  Intern      == Intern      = True
  (Line as)   == (Line bs)   = as == bs
  (Tag a x _) == (Tag b y _) = a == b && x == y
  _           == _           = False


------------------------------------------------------------------------------
-- | Record containing the additional JIRA information
-- of a specific issue
data JiraIssue = JiraIssue
  { jKey           :: Text
  , jSummary       :: Text
  , jToTest        :: Bool
  , jDocumentation :: Bool
  , jPR            :: Bool
  } deriving ( Eq, Ord, Show )


instance NFData JiraIssue


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


------------------------------------------------------------------------------
-- | Internal structure to hold the JSON information
-- of a JIRA custom field definition
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


------------------------------------------------------------------------------
-- | Record holding the application configuration
data Config = Config
  { cRange :: Maybe (String, String)
  , cPath  :: FilePath
  , cDate  :: UTCTime
  , cJira  :: String
  , cAuth  :: Maybe (BS.ByteString, BS.ByteString)
  } deriving ( Eq, Ord, Show )


------------------------------------------------------------------------------
-- | Default application configuration
defaultConfig :: UTCTime -> Config
defaultConfig d = Config
  { cRange = Nothing
  , cPath  = "."
  , cDate  = d
  , cJira  = []
  , cAuth  = Nothing
  }


------------------------------------------------------------------------------
-- | Does the configuration contain valid JIRA
-- information/credentials?
hasJira :: Config -> Bool
hasJira cfg =
  not (null jira) && hasAuth
 where
  jira    = cJira cfg
  hasAuth = isJust $ cAuth cfg


-- vim: set et sw=2 sts=2 tw=80:
