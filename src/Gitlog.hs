{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Time.Clock    ( getCurrentTime )

import           System.Console.GetOpt
import           System.IO          ( hPutStrLn, stderr )
import           System.Process     ( createProcess, proc, cwd, std_out
                                    , StdStream(..) )
import           System.Exit        ( ExitCode(..), exitWith, exitSuccess )
import           System.Environment ( getArgs )

import           Gitlog.Encoder
import           Gitlog.Jira
import           Gitlog.Parser
import           Gitlog.Types
import           Gitlog.Utils


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
      then toHtml cfg <$> getJiraInfo cfg x
      else return $ toHtml cfg x


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

  , Option "a" ["auth"]
    (ReqArg auth "USER:PW")
    "jira authentication (user:password)"

  , Option "h" ["help"]
    (NoArg
      (\_ -> do
        let prg = "gitlog [<from> [<to>]]"
        hPutStrLn stderr (usageInfo prg options)
        exitSuccess))
    "show this help"
  ]
 where
  getAuth x =
    case split ':' x of
      (u:p:_) -> Just (BS.pack u, BS.pack p)
      _       -> Nothing
  auth a opt =
    case getAuth a of
      (Just x) -> return $ opt { cAuth = Just x }
      _        -> return opt


main :: IO ()
main = do
  opts <- parseArgs =<< getArgs
  BL.putStr =<< getGitOutput opts (log' (range $ cRange opts))
 where
  log' a = "log" : "--pretty=format:|%h|%an|%ai|%s%n%b" : "--no-merges" : a

  range Nothing       = []
  range (Just (f, t)) = [f ++ ".." ++ t]


-- vim: set et sw=2 sts=2 tw=80:
