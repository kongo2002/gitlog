{-# LANGUAGE OverloadedStrings #-}

module Gitlog.Jira
  ( getJira
  ) where

import           Control.Applicative
import           Control.Exception      ( catch, SomeException(..) )
import           Control.Monad.Par.IO
import           Control.Monad.Par.Class
import           Control.Monad.IO.Class ( liftIO )

import           Data.Aeson             ( decode )
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BS
import           Data.List              ( intercalate, nub )
import qualified Data.Map.Strict as M
import           Data.Maybe             ( mapMaybe )

import           Network.HTTP.Conduit

import           System.IO              ( hPrint, stderr )

import           Gitlog.Types


------------------------------------------------------------------------------
-- | Retrieve all JIRA information for the given list of @GitEntry@
getJira :: Config -> [GitEntry] -> IO [GitEntry]
getJira cfg es = do
  issueMap <- M.fromList . mapMaybe issuesOnly <$> getJiraInfo cfg issues

  dPrint $ map (getIssues issueMap) es
 where
  dPrint x
    | cDebug cfg = hPrint stderr x >> return x
    | otherwise  = return x

  issues =
    nub $ foldr tag [] $ concatMap gBody es
   where
    tag x@Tag{} a = x:a
    tag _ a       = a

  issuesOnly (x, Just j) = Just (x, j)
  issuesOnly _           = Nothing

  getIssues m entry
    | null body = entry
    | otherwise = entry { gBody = map (getIssue m) body }
   where
    body = gBody entry

  getIssue m tag@(Tag t no _) = Tag t no (M.lookup tag m)
  getIssue _ x                = x


------------------------------------------------------------------------------
-- | Fetch all available JIRA information for the given list of
-- @GitBody@. The HTTP requests are fired concurrently.
getJiraInfo :: Config -> [GitBody] -> IO [(GitBody, Maybe JiraIssue)]
getJiraInfo cfg es = do
  mng <- liftIO $ newManager settings
  res <- runParIO (mapM get =<< mapM (go mng) es)
  closeManager mng
  return res
 where
  go :: Manager -> GitBody -> ParIO (IVar (GitBody, Maybe JiraIssue))
  go m tag = do
    i <- new
    fork (liftIO (go' m tag) >>= put i)
    return i

  go' :: Manager -> GitBody -> IO (GitBody, Maybe JiraIssue)
  go' m body = do
    jira <- safeFetch m cfg body
    return (body, jira)

  settings = conduitManagerSettings { managerConnCount = 100 }


------------------------------------------------------------------------------
-- | Safe HTTP request against the JIRA API
safeFetch :: Manager -> Config -> GitBody -> IO (Maybe JiraIssue)
safeFetch m cfg tag =
  fetch m cfg tag `catch` ex
 where
  ex (SomeException e) =
    hPrint stderr e >> return Nothing


------------------------------------------------------------------------------
-- | Fetch the desired information of the JIRA API
fetch :: Manager -> Config -> GitBody -> IO (Maybe JiraIssue)
fetch m cfg (Tag ty no _) =
  decode <$> httpTimeout cfg m url
 where
  tag  = BS.unpack ty ++ "-" ++ show no
  base = cJira cfg
  url  = base ++ "/rest/api/2/issue/" ++ tag ++ "?fields=" ++ fields
  fields = intercalate ","
    [ "summary"
    , "customfield_10411"
    , "customfield_10412"
    , "status"
    , "parent"
    ]

fetch _ _ _ = return Nothing


------------------------------------------------------------------------------
-- | HTTP request using a specific timeout
httpTimeout :: Config -> Manager -> String -> IO BL.ByteString
httpTimeout cfg manager url = do
  req <- applyBasicAuth user pw <$> parseUrl url
  let req' = req {responseTimeout = Just timeout}
  responseBody <$> httpLbs req' manager
 where
  (Just (user, pw)) = cAuth cfg

  -- timeout in microseconds (10 seconds)
  timeout = 10 * 1000 * 1000


-- vim: set et sw=2 sts=2 tw=80:
