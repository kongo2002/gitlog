module Gitlog where

import Control.Applicative

import System.IO          ( hGetContents, hPutStrLn, stderr )
import System.Process     ( runInteractiveProcess
                          , waitForProcess )
import System.Exit        ( ExitCode(..), exitWith )
import System.Environment ( getArgs )


getGitOutput :: FilePath -> [String] -> IO (String, ExitCode)
getGitOutput dir args = do
  (_in, out, _err, handle) <- runInteractiveProcess "git" args path Nothing
  ec <- waitForProcess handle
  output <- hGetContents out
  return (output, ec)
 where
  path = Just dir


range :: String -> String -> [String]
range f t = [f ++ ".." ++ t]


parseArgs :: [String] -> ([String], FilePath)
parseArgs [from, to, dir] = (range from to, dir)
parseArgs [from, to]      = (range from to, ".")
parseArgs [from]          = (range from "HEAD", ".")
parseArgs _               = ([], ".")


main :: IO ()
main = do
  (args, dir) <- parseArgs <$> getArgs
  (out, ec)   <- getGitOutput dir (log' args)
  case ec of
    ExitSuccess -> putStr out
    _           -> do
      hPutStrLn stderr "failed to retrieve git log"
      exitWith $ ExitFailure 1
 where
  log' a = "log" : "--pretty=format:|%h@%s%n%b" : a
