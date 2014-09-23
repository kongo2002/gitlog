{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Control.Exception  ( catch, SomeException(..) )

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Time.Clock    ( getCurrentTime )

import           Network.HTTP.Conduit

import           System.Console.GetOpt
import           System.IO          ( hPutStrLn, stderr )
import           System.Process     ( createProcess, proc, cwd, std_out
                                    , StdStream(..) )
import           System.Exit        ( ExitCode(..), exitWith, exitSuccess )
import           System.Environment ( getArgs )

import           Gitlog.Encoder
import           Gitlog.Parser
import           Gitlog.Types


getGitOutput :: Config -> [String] -> IO BL.ByteString
getGitOutput cfg args = do
  (_, Just out, _, _) <- createProcess prc { cwd = dir
                                           , std_out = CreatePipe }
  -- TODO: waitForProcess
  BL.hGetContents out >>= parse >>= html
 where
  dir = Just $ cPath cfg
  prc = proc "git" args

  intern Intern = True
  intern _      = False

  noIntern e = not $ any intern $ gBody e

  parse = return . filter noIntern . parseInput
  html x =
    case cJira cfg of
      [] -> return $ toHtml cfg x
      j  -> toHtml cfg <$> populateTags j x


populateTags :: String -> [GitEntry] -> IO [GitEntry]
populateTags base = mapM go
 where
  go entry =
    case body of
      [] -> return entry
      ls -> do
        body' <- mapM (safeFetch base) ls
        return $ entry { gBody = body' }
   where
    body = gBody entry


safeFetch :: String -> GitBody -> IO GitBody
safeFetch base tag =
  fetch base tag `catch` ex
 where
  ex (SomeException _) = return tag


fetch :: String -> GitBody -> IO GitBody
fetch base (Tag ty no _) = do
  mng <- newManager conduitManagerSettings
  res <- BL.toStrict <$> httpTimeout mng url tout
  return $ Tag ty no res
 where
  tag = BS.unpack ty ++ "-" ++ show no
  url = base ++ "/browse/" ++ tag

  -- timeout in microseconds
  tout = 1 * 1000 * 1000

fetch _ x = return x


httpTimeout :: Manager -> String -> Int -> IO BL.ByteString
httpTimeout manager url timeout = do
  req <- parseUrl url
  let req' = req {responseTimeout = Just timeout}
  responseBody <$> httpLbs req' manager


parseArgs :: [String] -> IO Config
parseArgs args = do
  date <- getCurrentTime
  let (actions, noOpt, _err) = getOpt Permute options args

  noopt noOpt <$> foldl (>>=) (return $ defaultConfig date) actions
 where
  noopt [f]     opts = opts { cRange = Just (f, "HEAD") }
  noopt (f:t:_) opts = opts { cRange = Just (f, t) }
  noopt _       opts = opts


exit :: String -> IO ()
exit msg =
  hPutStrLn stderr msg >> exitWith (ExitFailure 1)


options :: [ OptDescr (Config -> IO Config) ]
options =
  [ Option "f" ["from"]
    (ReqArg
      (\arg opt ->
        case cRange opt of
          (Just (_f, t)) -> return opt { cRange = Just (arg, t) }
          Nothing        -> return opt { cRange = Just (arg, "HEAD") })
      "COMMIT")
    "git commit to start from"

  , Option "t" ["to"]
    (ReqArg
      (\arg opt ->
        case cRange opt of
          (Just (f, _t)) -> return opt { cRange = Just (f, arg) }
          Nothing        -> return opt { cRange = Just ([], arg) })
      "COMMIT")
    "git commit to end with (default: HEAD)"

  , Option "d" ["dir"]
    (ReqArg
      (\arg opt -> return opt { cPath = arg })
      "PATH")
    "git directory"

  , Option "j" ["jira"]
    (ReqArg
      (\arg opt -> return opt { cJira = arg })
      "URL")
    "jira base URL"

  , Option "h" ["help"]
    (NoArg
      (\_ -> do
        let prg = "gitlog [<from> [<to>]]"
        hPutStrLn stderr (usageInfo prg options)
        exitSuccess))
    "show this help"
  ]


main :: IO ()
main = do
  opts <- parseArgs =<< getArgs
  BL.putStr =<< getGitOutput opts (log' (range $ cRange opts))
 where
  log' a = "log" : "--pretty=format:|%h|%an|%ai|%s%n%b" : "--no-merges" : a

  range Nothing       = []
  range (Just (f, t)) = [f ++ ".." ++ t]


-- vim: set et sw=2 sts=2 tw=80:
