{-# LANGUAGE OverloadedStrings #-}

module Gitlog.Types
  ( GitEntry(..)
  , GitBody(..)
  , Config(..)
  , JiraIssue(..)
  , OutputFormat(..)
  , defaultConfig
  , hasJira
  ) where

import           Control.Applicative
import           Control.Monad      ( mzero )
import           Control.Parallel.Strategies
import           Data.Aeson
import qualified Data.ByteString.Char8 as BS
import           Data.Time.Clock    ( UTCTime )
import           Data.Maybe         ( isJust, catMaybes )
import           Data.Monoid        ( mempty )
import           Data.Text          ( Text )
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Data.Text.Encoding ( decodeUtf8 )
import           Data.Text.Lazy.Builder
import qualified Data.Vector as V

import           Gitlog.Utils


------------------------------------------------------------------------------
-- | Record describing one git commit
data GitEntry = GitEntry
  { gSHA    :: BS.ByteString
  , gAuthor :: BS.ByteString
  , gDate   :: BS.ByteString
  , gTitle  :: BS.ByteString
  , gBody   :: [GitBody]
  } deriving ( Eq, Ord, Show )


instance ToJSON GitEntry where
  toJSON e =
    object
      [ "commit"   .= decodeUtf8 (gSHA e)
      , "author"   .= decodeUtf8 (gAuthor e)
      , "date"     .= decodeUtf8 (gDate e)
      , "title"    .= decodeUtf8 (gTitle e)
      , "body"     .= object (catMaybes
        [ nonEmpty "text" (toLazyText text)
        , nonNull "tags" tags
        , Just ("intern" .= intern)
        ])
      ]
   where
    (text, tags, intern) = foldr go (mempty, [], False) $ gBody e

    -- group body lines into one text entry
    -- and aggregate all tag entries
    go (Line x) (ls, ts, i) = (fromText (decodeUtf8 x) <> "\n" <> ls, ts, i)
    go t@Tag{}  (ls, ts, i) = (ls, tag t : ts, i)
    go Intern   (ls, ts, _) = (ls, ts, True)

    tag (Tag t x jira) =
      case jira of
        (Just j) -> object ["name" .= name, "jira" .= j]
        Nothing  -> object ["name" .= name]
     where
      name = T.pack (BS.unpack t ++ "-" ++ show x)
    tag _ = Null

    nonEmpty k v
      | TL.null v = Nothing
      | otherwise = Just (k .= v)

    nonNull _ [] = Nothing
    nonNull k vs = Just (k .= vs)


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
  , jStatus        :: JiraStatusCategoryType
  , jParent        :: Maybe JiraIssue
  } deriving ( Eq, Ord, Show )


instance NFData JiraIssue


instance FromJSON JiraIssue where
  parseJSON (Object v) = do
    key <- v .: "key"
    fs  <- v .: "fields"
    (s, tt, doc, pr, st, p) <- fields fs
    return $ JiraIssue key s tt doc pr st p
   where
    fields (Object x) = do
      s  <- x .: "summary"
      -- "to be tested"
      tt <- exists =<< (x .:? "customfield_10411" .!= Null)
      -- "documentation relevant" and "PR relevant"
      cs <- toList =<< (x .:? "customfield_10412" .!= Null)
      -- issue status
      st <- status =<< x .: "status"
      -- optional issue parent
      p  <- x .:? "parent"
      return (s, tt, hasField "10123" cs, hasField "10220" cs, st, p)
    fields _ = mzero

    exists Null = return False
    exists _    = return True

    hasField i = any ((== i) . cfId)

    toList (Array xs) = mapM parseJSON $ V.toList xs
    toList _          = return []

    status (Object s) = (idToType . jscId) <$> s .: "statusCategory"
    status _          = mzero

  parseJSON _ = mzero


instance ToJSON JiraIssue where
  toJSON (JiraIssue k s tt d pr _ _) =
    object
     [ "key" .= k
     , "summary" .= s
     , "toTest" .= tt
     , "documentation" .= d
     , "pr" .= pr
     ]


------------------------------------------------------------------------------
-- | Internal structure to hold the JSON information
-- of a JIRA custom field definition
data CustomField = CI
  { cfId     :: Text
  , _cfSelf  :: Text
  , _cfValue :: Text
  }


instance FromJSON CustomField where
  parseJSON (Object o) =
    CI <$> o .: "id"
       <*> o .: "self"
       <*> o .: "value"
  parseJSON _ = mzero


data JiraStatusCategoryType =
    NoCategory
  | New
  | Complete
  | InProgress
  deriving ( Show, Eq, Ord )


idToType :: Int -> JiraStatusCategoryType
idToType 1 = NoCategory
idToType 2 = New
idToType 3 = Complete
idToType 4 = InProgress
idToType _ = NoCategory


data JiraStatusCategory = JSC
  { jscId   :: Int
  , _jscKey :: Text
  }


instance FromJSON JiraStatusCategory where
  parseJSON (Object x) =
    JSC <$> x .: "id" <*> x .: "key"
  parseJSON _ = mzero


------------------------------------------------------------------------------
-- | Output format used in encoding
data OutputFormat =
    Html
  | Json
  deriving ( Eq, Show )


------------------------------------------------------------------------------
-- | Record holding the application configuration
data Config = Config
  { cRange  :: Maybe (String, String)
  , cPath   :: FilePath
  , cDate   :: UTCTime
  , cJira   :: String
  , cAuth   :: Maybe (BS.ByteString, BS.ByteString)
  , cOutput :: OutputFormat
  , cDebug  :: Bool
  } deriving ( Eq, Show )


------------------------------------------------------------------------------
-- | Default application configuration
defaultConfig :: UTCTime -> Config
defaultConfig d = Config
  { cRange  = Nothing
  , cPath   = "."
  , cDate   = d
  , cJira   = []
  , cAuth   = Nothing
  , cOutput = Html
  , cDebug  = False
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
