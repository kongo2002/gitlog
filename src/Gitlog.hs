{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative

import qualified Data.ByteString.Lazy as BL

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
  output <- proc <$> BL.hGetContents out
  ec <- waitForProcess h
  return (output, ec)
 where
  path = Just dir
  proc = toHtml . filter noIntern . parseInput

  intern Intern = True
  intern _      = False

  noIntern e = not $ any intern $ gBody e


range :: String -> String -> [String]
range f t = [f ++ ".." ++ t]


parseArgs :: [String] -> ([String], FilePath)
parseArgs [from, to, dir] = (range from to, dir)
parseArgs [from, to]      = (range from to, ".")
parseArgs [from]          = (range from "HEAD", ".")
parseArgs _               = ([], ".")


exit :: String -> IO ()
exit msg =
  hPutStrLn stderr msg >> exitWith (ExitFailure 1)


main :: IO ()
main = do
  (args, dir) <- parseArgs <$> getArgs
  (out, ec)   <- getGitOutput dir (log' args)
  case ec of
    ExitSuccess -> BL.putStr out
    _           -> exit "failed to retrieve git log"
 where
  log' a = "log" : "--pretty=format:|%h|%an|%ai|%s%n%b" : "--no-merges" : a


-- vim: set et sw=2 sts=2 tw=80:
