{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import           Control.Applicative

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.List          ( groupBy, nub, find, sortBy )
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
getGitEntries :: Config -> IO (ExitCode, [GitEntry])
getGitEntries cfg = do
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

  range =
    case cRange cfg of
      Nothing       -> []
      (Just (f, t)) -> [f ++ ".." ++ t]

  args = "--no-pager"
       : "log"
       : "--pretty=format:|%h|%an|%ai|%s%n%b"
       : "--no-merges"
       : range

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
getOutput cfg entries
  | debug       = return $ LBS.pack $ show entries
  | hasJira cfg = encode cfg . groupIssues <$> getJira cfg entries
  | otherwise   = return $ encode cfg entries
 where
  debug = cDebug cfg
  tags a b =
    case (firstTag a, firstTag b) of
      (Just t1, Just t2) -> compare t2 t1
      (Just _, _)        -> LT
      (_, Just _)        -> GT
      _                  -> compare (gDate a) (gDate b)

  firstTag = find isTag . gBody

  sameJira a b =
    any (`elem` b_tags) a_tags
   where
    onlyTags = filter isTag
    a_tags   = onlyTags $ gBody a
    b_tags   = onlyTags $ gBody b

  groupIssues = map select . groupBy sameJira . sortBy tags
   where
    select []           = error "captain! we've been hit"
    select [x]          = x
    select xs@(first:_) =
      let body = nub $ filter isTag $ concatMap gBody xs
      in  first { gBody = body }


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

  , Option [] ["format"]
    (ReqArg format "FORMAT")
    "output format (either html or json)"

  , Option "j" ["jira"]
    (ReqArg
      (\arg opt -> return opt { cJira = arg })
      "URL")
    "jira base URL"

  , Option "a" ["auth"]
    (ReqArg auth "USER:PW")
    "jira authentication (user:password)"

  , Option [] ["debug"]
    (NoArg (\o -> return o { cDebug = True }))
    "enable debug output"

  , Option "V" ["version"]
    (NoArg (\_ -> hPutStrLn stderr ("gitlog " ++ getVersion) >> exitSuccess))
    "show version"

  , Option "h" ["help"]
    (NoArg
      (\_ -> do
        let prg = "Usage: gitlog [<from> [<to>]]\n"
        hPutStrLn stderr (usageInfo prg options)
        exitSuccess))
    "show this help"
  ]
 where
  auth x opt =
    case split ':' x of
      (u:p:_) -> let a = Just (BS.pack u, BS.pack p)
                 in return opt { cAuth = a }
      _       -> return opt

  format x opt =
    case x of
      "json" -> return opt { cOutput = Json }
      "html" -> return opt { cOutput = Html }
      _      -> return opt


------------------------------------------------------------------------------
-- | Main application entry point
main :: IO ()
main = do
  opts <- parseArgs =<< getArgs
  (ec, result) <- getGitEntries opts
  case ec of
    ExitSuccess -> getOutput opts result >>= BL.putStr
    _           -> exitWith ec


-- vim: set et sw=2 sts=2 tw=80:
