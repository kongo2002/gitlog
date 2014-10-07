{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import           Data.List          ( groupBy, intercalate )
import           Data.Time.Clock    ( getCurrentTime )

import           System.Console.GetOpt
import           System.IO          ( hPutStrLn, stderr )
import           System.Process     ( createProcess, proc, cwd, std_out
                                    , StdStream(..), waitForProcess )
import           System.Exit        ( ExitCode(..), exitWith, exitSuccess )
import           System.Environment ( getArgs )

import           Gitlog.Encoder
import           Gitlog.Jira
import           Gitlog.Parser
import           Gitlog.Types
import           Gitlog.Utils


------------------------------------------------------------------------------
-- | Shell out git process and parse the results into a list of @GitEntry@
getGitEntries :: Config -> [String] -> IO (ExitCode, [GitEntry])
getGitEntries cfg args = do
  (_, Just out, _, h) <- createProcess prc { cwd = dir
                                           , std_out = CreatePipe }
  -- force evaluation of process output
  -- otherwise 'waitForOutput' would block endlessly
  !result <- parse <$> BL.hGetContents out
  ec <- waitForProcess h
  return (ec, result)
 where
  dir = Just $ cPath cfg
  prc = proc "git" args

  intern Intern = True
  intern _      = False

  relevant e = noIntern e && noRevert e

  noIntern e = not $ any intern $ gBody e
  noRevert e =
    let title = BS.unpack $ gTitle e
    in not $ startsWith "Revert" title

  parse x =
    let res = filter relevant $ parseInput x
        len = length res
    in  len `seq` res


------------------------------------------------------------------------------
-- | Convert the given list of @GitEntry@ into a lazy bytestring output
getOutput :: Config -> [GitEntry] -> IO BL.ByteString
getOutput cfg entries =
  if hasJira cfg
    then toHtml cfg <$> withJira entries
    else return $ toHtml cfg entries
 where
  withJira x =
    groupIssues <$> getJiraInfo cfg x

  sameJira a b =
    any (`elem` b_tags) a_tags
   where
    onlyTags = filter isTag
    a_tags   = onlyTags $ gBody a
    b_tags   = onlyTags $ gBody b

  groupIssues = map select . groupBy sameJira
   where
    select []       = error "captain! we've been hit"
    select xs@(x:_) =
      let sep = [Line ""]
          body = intercalate sep $ map gBody xs
      in  x { gBody = body }


isTag :: GitBody -> Bool
isTag Tag{} = True
isTag _     = False


------------------------------------------------------------------------------
-- | Parse command line arguments
parseArgs :: [String] -> IO Config
parseArgs args = do
  date <- getCurrentTime
  let (actions, noOpt, _err) = getOpt Permute options args

  noopt noOpt <$> foldl (>>=) (return $ defaultConfig date) actions
 where
  noopt [f]     opts = opts { cRange = Just (f, "HEAD") }
  noopt (f:t:_) opts = opts { cRange = Just (f, t) }
  noopt _       opts = opts


------------------------------------------------------------------------------
-- | Exit with a stderr error message
exit :: String -> IO ()
exit msg =
  hPutStrLn stderr msg >> exitWith (ExitFailure 1)


------------------------------------------------------------------------------
-- | Command line argument description
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
  auth x opt =
    case split ':' x of
      (u:p:_) -> let a = Just (BS.pack u, BS.pack p)
                 in return $ opt { cAuth = a }
      _       -> return opt


------------------------------------------------------------------------------
-- | Main application entry point
main :: IO ()
main = do
  opts <- parseArgs =<< getArgs
  (ec, result) <-  getGitEntries opts (log' (range $ cRange opts))
  case ec of
    ExitSuccess -> getOutput opts result >>= BL.putStr
    _           -> exitWith ec
 where
  log' a = "--no-pager" : "log" : "--pretty=format:|%h|%an|%ai|%s%n%b" : "--no-merges" : a

  range Nothing       = []
  range (Just (f, t)) = [f ++ ".." ++ t]


-- vim: set et sw=2 sts=2 tw=80:
