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
getJiraInfo cfg es =
  runParIO (mapM get =<< mapM go es)
 where
  go :: GitEntry -> ParIO (IVar GitEntry)
  go tag = do
    i <- new
    fork (do xx <- liftIO (go' tag)
             put i xx)
    return i

  go' :: GitEntry -> IO GitEntry
  go' entry =
    case body of
      [] -> return entry
      ls -> do
        body' <- mapM (safeFetch cfg) ls
        return $ entry { gBody = body' }
   where
    body = gBody entry


safeFetch :: Config -> GitBody -> IO GitBody
safeFetch cfg tag =
  fetch cfg tag `catch` ex
 where
  ex (SomeException _) = return tag


fetch :: Config -> GitBody -> IO GitBody
fetch cfg (Tag ty no _) = do
  mng <- newManager conduitManagerSettings
  res <- decode <$> httpTimeout cfg mng url tout
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

fetch _ x = return x


httpTimeout :: Config -> Manager -> String -> Int -> IO BL.ByteString
httpTimeout cfg manager url timeout = do
  req <- applyBasicAuth user pw <$> parseUrl url
  let req' = req {responseTimeout = Just timeout}
  responseBody <$> httpLbs req' manager
 where
  (Just (user, pw)) = cAuth cfg


-- vim: set et sw=2 sts=2 tw=80:
