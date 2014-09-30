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

import           System.IO          ( hPrint, stderr )

import           Gitlog.Types


getJiraInfo :: Config -> [GitEntry] -> IO [GitEntry]
getJiraInfo cfg es = do
  mng <- liftIO $ newManager (conduitManagerSettings { managerConnCount = 100})
  res <- runParIO (mapM get =<< mapM (go mng) es)
  closeManager mng
  return res
 where
  go :: Manager -> GitEntry -> ParIO (IVar GitEntry)
  go m tag =
    if noBody
      then newFull tag
      else do
        i <- new
        fork (liftIO (go' m tag) >>= put i)
        return i
   where
    noBody = null $ gBody tag

  go' :: Manager -> GitEntry -> IO GitEntry
  go' m entry = do
    body <- mapM (safeFetch m cfg) (gBody entry)
    return $ entry { gBody = body }


safeFetch :: Manager -> Config -> GitBody -> IO GitBody
safeFetch m cfg tag =
  fetch m cfg tag `catch` ex
 where
  ex (SomeException e) =
    hPrint stderr e >> return tag


fetch :: Manager -> Config -> GitBody -> IO GitBody
fetch m cfg (Tag ty no _) = do
  out <- httpTimeout cfg m url
  let res = decode out
  hPrint stderr res
  return $ Tag ty no res
 where
  tag  = BS.unpack ty ++ "-" ++ show no
  base = cJira cfg
  url  = base ++ "/rest/api/2/issue/" ++ tag ++ "?fields=" ++ fields
  fields = intercalate ","
    [ "summary"
    , "customfield_10411"
    , "customfield_10412"
    ]

fetch _ _ x = return x


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
