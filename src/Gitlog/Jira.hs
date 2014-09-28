{-# LANGUAGE OverloadedStrings #-}

module Gitlog.Jira
  ( getJiraInfo
  ) where

import           Control.Applicative
import           Control.Exception ( catch, SomeException(..) )
import           Control.Monad.Par.IO
import           Control.Monad.Par.Class
import           Control.Monad.IO.Class ( liftIO )

import           Data.Aeson        ( decode )
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BS
import           Data.List         ( intercalate )

import           Network.HTTP.Conduit

import           Gitlog.Types


getJiraInfo :: Config -> [GitEntry] -> IO [GitEntry]
getJiraInfo cfg es = do
  mng <- liftIO $ newManager conduitManagerSettings
  runParIO (mapM get =<< mapM (go mng) es)
 where
  go :: Manager -> GitEntry -> ParIO (IVar GitEntry)
  go m tag = do
    i <- new
    fork (liftIO (go' m tag) >>= put i)
    return i

  go' :: Manager -> GitEntry -> IO GitEntry
  go' m entry =
    case body of
      [] -> return entry
      ls -> do
        body' <- mapM (safeFetch m cfg) ls
        return $ entry { gBody = body' }
   where
    body = gBody entry


safeFetch :: Manager -> Config -> GitBody -> IO GitBody
safeFetch m cfg tag =
  fetch m cfg tag `catch` ex
 where
  ex (SomeException _) = return tag


fetch :: Manager -> Config -> GitBody -> IO GitBody
fetch m cfg (Tag ty no _) = do
  res <- decode <$> httpTimeout cfg m url tout
  return $ Tag ty no res
 where
  tag  = BS.unpack ty ++ "-" ++ show no
  base = cJira cfg
  url  = base ++ "/rest/api/2/issue/" ++ tag ++ "?fields=" ++ fields
  fields = intercalate ","
    [ "summary"
    , "customfield_10411"
    ]

  -- timeout in microseconds
  tout = 1 * 1000 * 1000

fetch _ _ x = return x


httpTimeout :: Config -> Manager -> String -> Int -> IO BL.ByteString
httpTimeout cfg manager url timeout = do
  req <- applyBasicAuth user pw <$> parseUrl url
  let req' = req {responseTimeout = Just timeout}
  responseBody <$> httpLbs req' manager
 where
  (Just (user, pw)) = cAuth cfg


-- vim: set et sw=2 sts=2 tw=80:
