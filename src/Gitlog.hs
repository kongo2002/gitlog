{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Control.Exception  ( catch, SomeException(..) )

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Time.Clock    ( getCurrentTime )

import           Network.HTTP       ( simpleHTTP, getRequest, rspBody )

import           System.Console.GetOpt
import           System.IO          ( hPutStrLn, stderr )
import           System.Process     ( runInteractiveProcess, waitForProcess )
import           System.Exit        ( ExitCode(..), exitWith )
import           System.Environment ( getArgs )

import           Gitlog.Encoder
import           Gitlog.Parser
import           Gitlog.Types


getGitOutput :: FilePath -> [String] -> IO (BL.ByteString, ExitCode)
getGitOutput dir args = do
  (_in, out, _err, h) <- runInteractiveProcess "git" args path Nothing
  output <- BL.hGetContents out >>= parse >>= html
  ec     <- waitForProcess h
  return (output, ec)
 where
  path = Just dir

  intern Intern = True
  intern _      = False

  noIntern e = not $ any intern $ gBody e

  parse  = return . filter noIntern . parseInput
  html x = toHtml <$> populateTags x


populateTags :: [GitEntry] -> IO [GitEntry]
populateTags = mapM go
 where
  go entry =
    case body of
      [] -> return entry
      ls -> do
        body' <- mapM populate ls
        return $ entry { gBody = body' }
   where
    body = gBody entry


populate :: GitBody -> IO GitBody
populate tag =
  populate' tag `catch` ex
 where
  ex (SomeException _) = return tag


populate' :: GitBody -> IO GitBody
populate' t@(Tag ty no _) = do
  res <- simpleHTTP $ getRequest url
  case res of
    (Right r) -> do
      let body = rspBody r
      putStr body
      return $ Tag ty no body
    _ -> return t
 where
  tag = BS.unpack ty ++ "-" ++ show no
  url = "http://localhost:9999/browse/" ++ tag
populate' x = return x


parseArgs :: [String] -> IO Config
parseArgs args = do
  date <- getCurrentTime
  let (actions, noOpt, _err) = getOpt RequireOrder options args

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

  , Option "h" ["help"]
    (NoArg
      (\_ -> do
        let prg = "gitlog [<from> [<to>]]"
        hPutStrLn stderr (usageInfo prg options)
        exitWith ExitSuccess))
    "show this help"
  ]


main :: IO ()
main = do
  opts      <- parseArgs =<< getArgs
  (out, ec) <- getGitOutput (cPath opts) (log' (range $ cRange opts))
  case ec of
    ExitSuccess -> BL.putStr out
    _           -> exit "failed to retrieve git log"
 where
  log' a = "log" : "--pretty=format:|%h|%an|%ai|%s%n%b" : "--no-merges" : a

  range Nothing       = []
  range (Just (f, t)) = [f ++ ".." ++ t]


-- vim: set et sw=2 sts=2 tw=80:
