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
    if hasJira cfg
      then return $ toHtml cfg x
      else toHtml cfg <$> populateTags cfg x


populateTags :: Config -> [GitEntry] -> IO [GitEntry]
populateTags cfg = mapM go
 where
  go entry =
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
  res <- BL.toStrict <$> httpTimeout cfg mng url tout
  return $ Tag ty no res
 where
  tag  = BS.unpack ty ++ "-" ++ show no
  base = cJira cfg
  url  = base ++ "/browse/" ++ tag

  -- timeout in microseconds
  tout = 1 * 1000 * 1000

fetch _ x = return x


httpTimeout :: Config -> Manager -> String -> Int -> IO BL.ByteString
httpTimeout cfg manager url timeout = do
  req <- applyBasicAuth user pw <$> parseUrl url
  let req' = req {responseTimeout = Just timeout}
  responseBody <$> httpLbs req' manager
 where
  (Just a) = cAuth cfg
  user     = BS.pack $ fst a
  pw       = BS.pack $ snd a


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
